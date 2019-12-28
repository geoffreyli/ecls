setwd("~/geoffli @ UW/19 Wi - DATA 557/Final Project/ecls")

library(tidyverse)
library(sandwich)
library(lmtest)
library(haven)
library(sjmisc)

qqn <- function (x, ...) {
  qqnorm(x, ...)
  qqline(x)
}

# Read in raw data
dta <- read_por('data-raw/04075-0001-Data.por')

vars_chrs <- c("CHILDID",
               "W3INCCAT",
               "P5GOTOBD") # added

vars_nums <- c("C5R2MTSC",
               "C5R2RTSC",
               "C5STSCOR",
               "P5TIMEHR")

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


bedtime <- dta %>%
  select(vars_chrs, vars_nums)

# Data cleaning
bedtime <- bedtime %>%
  mutate(
    BedTime = case_when(
      is.na(P5TIMEHR) ~ as.numeric(NA),
      P5TIMEHR < 0 ~ as.numeric(NA),
      P5TIMEHR < 6 ~ as.numeric(NA),
      TRUE ~ P5TIMEHR
    ),
    BedTimeConsistent = case_when(
      is.na(P5GOTOBD) ~ as.numeric(NA),
      P5GOTOBD == "HAS USUAL BEDTIME" ~ 1,
      P5GOTOBD == "BEDTIME VARIES" ~ 0,
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
    ScienceTScore = case_when(
      is.na(C5STSCOR) ~ as.numeric(NA),
      C5STSCOR < 0 ~ as.numeric(NA),
      TRUE ~ C5STSCOR
    ),
    Income = W3INCCAT
  ) %>%
  select(CHILDID, Income, BedTime, BedTimeConsistent, MathTScore, ReadTScore, ScienceTScore)


# Drop rows with NAs
# bedtime <- bedtime %>%
#   drop_na()

# Write data to CSV
bedtime %>%
  write_csv("./data-processed/bedtime.csv")

summary(bedtime)


# Statistical Tests

summary(lm(ReadTScore ~ BedTime, data=bedtime))

summary(lm(MathTScore ~ BedTime, data=bedtime))

summary(lm(ScienceTScore ~ BedTime, data=bedtime))



summary(lm(ReadTScore ~ BedTimeConsistent, data=bedtime))

summary(lm(MathTScore ~ BedTimeConsistent, data=bedtime))

summary(lm(ScienceTScore ~ BedTimeConsistent, data=bedtime))


summary(bedtime)
sd(bedtime$ScienceTScore, na.rm=TRUE)


# Assumptions
hist(bedtime$ReadTScore)
qqn(bedtime$ReadTScore)





