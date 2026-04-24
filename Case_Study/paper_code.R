# Code in paper
# Does not match code on GitHub which is much more complicated


# Data Prep ---------------------------------------------------------------

# Comes from 10-data-prep.R

library(httr2)
library(jsonlite)
library(data.table)
library(lubridate)
library(excessmort)
library(tidyverse)


## Get population from United States Census
source("Case_Study/census-key.R")  

api <- "https://api.census.gov/data/2019/pep/population"
pop1 <- request(api) |>  
  req_url_query(get = I("POP,DATE_CODE,DATE_DESC"), 
                `for` = I("us:*"),
                key = census_key) |>
  req_perform() |>
  resp_body_string() |> 
  fromJSON(flatten = TRUE) |>
  as.data.table()

pop1 <- pop1 |> janitor::row_to_names(1) 
pop1 <- pop1[!grepl("4/1", DATE_DESC)]
pop1 <- data.table(year = 2010 + as.numeric(pop1$DATE_CODE) - 3, population = as.numeric(pop1$POP))

pop2 <- fread("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/state/totals/NST-EST2024-ALLDATA.csv") 
years <- 2020:2024
pop2 <- pop2[NAME == "United States", ]
cols <- paste0("POPESTIMATE", years)
pop2 <- data.table(year = years, population = unlist(pop2[,..cols]))

pop <- rbindlist(list(pop1,pop2))[order(year)]

# CF: I had to add this
dat <- read.csv("Case_Study/CDC_Wonder_Data_Locked_Nov_28_2025/Multiple Cause of Death, 1999-2020.csv")
dat$Notes <- NULL
dat <- subset(dat, !is.na(Year))
dat <- subset(dat, Month != "")
names(dat)[c(1,3)] <- c("year", "month")


# CF: I had to change this
dat <- merge(dat, pop, by = "year", all.x = TRUE)
dat$month<- month(my(dat$month))

## Create additional variables for analysis
# not sure why?
dat <- dat %>%
  mutate(date_yr_mnth = make_date(year = year, month = month))

# test data for 2020 - 2024
years <- rep(2020:2024, each = 12)
months <- rep(1:12, 5)
test <- data.frame(year = years, month = months)

# save dat
save(dat, test, file = "data/data.Rdata")

# Read in data ------------------------------------------------------------

load(file = "data/data.Rdata")


# WHO's method ------------------------------------------------------------

library(mgcv)
model <- gam(Deaths ~ s(year, k = length(unique(dat$year)),bs = "tp") + 
               s(month, k = length(unique(dat$month)), bs = "cc"), 
             data = dat, 
             family = nb(theta = NULL, link = "log"))
predictions <- predict(model, se.fit = FALSE, type = "response", 
                       newdata = test)

# obtain uncertainty intervals
pred_log <- predict(model, se.fit = TRUE, 
                       newdata = test)
month_ints <- list()

# 60 = number of months for which we want to estimate excess death
for(j in 1:60){
  S <- 1000
  # S = no. samples for approximation
  eta <- pred_log$fit[j]
  sig <- pred_log$se.fit[j]
  samps <- exp(rnorm(S, mean = eta, sd = sig))
  m <- mean(samps)
  v <- var(samps)
  T_hat <- m^2/v
  E_hat <- m
  g_samps <- rgamma(S, shape = T_hat, rate = T_hat / E_hat)
  month_ints[[j]] <- c(quantile(g_samps, 0.025), quantile(g_samps, 0.0975))
}

# check some intervals
month_ints[[1]]
month_ints[[36]]

# place in a data frame
dplyr::bind_rows(month_ints) |> 
  mutate(year = test$year, month = test$month) |> 
  View()


# Economist's method ------------------------------------------------------

dat$days_in_month <- days_in_month(dat$month)
dat$deaths_per_day <- dat$Deaths / dat$days_in_month

expected_mod <- lm(deaths_per_day ~ year + factor(month), 
                   data = dat)

expected_est <- predict(expected_mod, newdata = test, 
                        interval = "prediction") * dat$days_in_month[1:12]
