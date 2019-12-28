setwd("~/geoffli @ UW/19 Wi - DATA 557/Final Project/ecls")

library(tidyverse)
library(sandwich)
library(lmtest)
library(haven)
library(sjmisc)

# Read in raw data
dta <- read_por('data-raw/04075-0001-Data.por')

vars_chrs <- c("CHILDID",
               "R5RACE",
               "R5REGION",
               "W3INCCAT",
               "S5MINOR",
               "S5SCTYP",
               "R5URBAN") # added

vars_nums <- c("C5R2MTSC",
               "C5R2RTSC")

dta <- dta %>%
  mutate_at(vars(one_of(vars_chrs)), funs(as.character(to_label(.)))) %>%
  mutate_at(vars(one_of(vars_nums)), funs(as.numeric(as.character(to_label(.)))))

# dta <- zap_formats(dta)
# dta <- zap_labels(dta)


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

# Select columns for Hypothesis 2 - Diversity
diversity <- dta %>%
  select(vars_chrs, vars_nums)

# Data cleaning
diversity <- diversity %>%
  mutate(
    DiversityQuartile = case_when(
      is.na(S5MINOR) ~ as.numeric(NA),
      S5MINOR == "NOT ASCERTAINED" ~ as.numeric(NA),
      S5MINOR == "NOT APPLICABLE" ~ as.numeric(NA),
      S5MINOR == "LESS THAN 10%" ~ 1,
      S5MINOR == "10% TO LESS THAN 25%" ~ 2,
      S5MINOR == "25% TO LESS THAN 50%" ~ 2,
      S5MINOR == "50% TO LESS THAN 75%" ~ 3,
      S5MINOR == "75% OR MORE" ~ 4,
      TRUE ~ as.numeric(NA)
    ),
    Race = as.factor(case_when(
      is.na(R5RACE) ~ as.character(NA),
      R5RACE == "NOT ASCERTAINED" ~ as.character(NA),
      TRUE ~ R5RACE
    )),
    Region = as.factor(case_when(
      is.na(R5REGION) ~ as.character(NA),
      R5REGION == "NOT ASCERTAINED" ~ as.character(NA),
      R5REGION == "NOT APPLICABLE" ~ as.character(NA),
      TRUE ~ R5REGION
    )), 
    SchoolType = as.factor(case_when(
      is.na(S5SCTYP) ~ as.character(NA),
      S5SCTYP == "NOT ASCERTAINED" ~ as.character(NA),
      S5SCTYP == "NOT APPLICABLE" ~ as.character(NA),
      TRUE ~ S5SCTYP
    )), 
    SchoolLocale = as.factor(case_when(
      is.na(R5URBAN) ~ as.character(NA),
      R5URBAN == "NOT ASCERTAINED" ~ as.character(NA),
      R5URBAN == "NOT APPLICABLE" ~ as.character(NA),
      TRUE ~ R5URBAN
    )), 
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
    Income = W3INCCAT
  ) %>%
  select(CHILDID, DiversityQuartile, Race, Region, Income, SchoolType, SchoolLocale, MathTScore, ReadTScore)


# Drop rows with NAs
diversity <- diversity %>%
  drop_na()

# Write data to CSV
diversity %>%
  write_csv("./data-processed/diversity.csv")

summary(diversity)


summary(lm(ReadTScore ~ DiversityQuartile*factor(SchoolLocale)*factor(SchoolType) + factor(Region) + Income , data=diversity))






# Descriptive Statistics

diversity %>%
  group_by(DiversityQuartile) %>%
  summarise(
    n = n(),
    ReadTScore = mean(ReadTScore),
    MathTScore = mean(MathTScore)
  )

diversity %>%
  group_by(Race) %>%
  summarise(
    n = n(),
    ReadTScore = mean(ReadTScore),
    MathTScore = mean(MathTScore)
  )




