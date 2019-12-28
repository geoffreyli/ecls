setwd("~/geoffli @ UW/19 Wi - DATA 557/Final Project/ecls")

library(tidyverse)
library(sandwich)
library(lmtest)
library(haven)
library(sjmisc)

# Read in raw data
dta <- read_por('data-raw/04075-0001-Data.por')

vars_chrs <- c("CHILDID",
               "W3INCCAT",
               "P5NOINSU") # added

vars_nums <- c("C5R2MTSC",
               "C5R2RTSC")

dta <- dta %>%
  mutate_at(vars(one_of(vars_chrs)), funs(as.character(to_label(.)))) %>%
  mutate_at(vars(one_of(vars_nums)), funs(as.numeric(as.character(to_label(.)))))


# Recode income categories to numeric. Income categories are set at
# their midvalue. 5000 or less is set to 5000; 200,001 or more is set
# to 200,001.
dta <- dta %>%
  mutate(W3INCCAT = if_else(
    W3INCCAT == '$5,000 OR LESS', '$5,000 TO $5,000', if_else(
      W3INCCAT == '$200,001 OR MORE', '$200,001 TO $200,001', W3INCCAT)
    )
  )

convert_income <- function(s) {                              # function for converting income
  split_mat <- str_split_fixed(s, " TO ", n = 2)
  split_mat <- gsub('\\$|,', '', split_mat)
  (as.numeric(split_mat[, 1]) + as.numeric(split_mat[, 2])) / 2
}

dta <- dta %>% mutate(W3INCCAT = convert_income(W3INCCAT))   # finally, convert income


health <- dta %>%
  select(vars_chrs, vars_nums)

# Data cleaning
health <- health %>%
  mutate(
    HealthInsurance = case_when(
      is.na(P5NOINSU) ~ as.numeric(NA),
      P5NOINSU == "NO" ~ 1,
      P5NOINSU == "YES" ~ 0,
      TRUE ~ as.numeric(NA)
    ),
    MathTScore = case_when(
      is.na(C5R2MTSC) ~ as.numeric(NA),
      C5R2MTSC < 0 ~ as.numeric(NA),
      TRUE ~ C5R2MTSC
    ),
    ReadTScore = case_when(
      is.na(C5R2RTSC) ~ as.numeric(NA),
      C5R2RTSC < 0 ~ as.numeric(NA),
      TRUE ~ C5R2RTSC
    ),
    Income = W3INCCAT,
    IncomeBracket = (case_when(
      is.na(Income) ~ as.character(NA),
      Income > 200000 ~ ">$200K",
      Income > 150000 ~ "$150K - $200K",
      Income > 100000 ~ "$100K - $150K",
      Income > 50000 ~ "$50K - $100K",
      TRUE ~ "<$50K"
    ))
  ) %>%
  select(CHILDID, HealthInsurance, Income, IncomeBracket, MathTScore, ReadTScore)


# Drop rows with NAs
health <- health %>%
  drop_na()

# Write data to CSV
health %>%
  write_csv("./data-processed/health.csv")

summary(health)


summary(lm(ReadTScore ~ HealthInsurance + Income, data=health))



