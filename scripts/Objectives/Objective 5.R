################################################################################
# Objective 5 - 
# Estimate the age, length, and sex composition by port of the lingcod harvest 
# landed at Kodiak, Homer, Seward, Whittier, and Valdez during July through September 
# such that the estimated proportions are within 0.20 of the true proportions at least 
# 90% of the time.
#
# Created using code from Objectives 1/2 (Halibut) by CWM 02/04/25
################################################################################
library(tidyverse)
library(gt)
source("functions.R")


get_data("data/LC/")


###

library(plyr)
ling <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008,
                                 ling2009, ling2010, ling2011, ling2012, ling2013,
                                 ling2014, ling2015, ling2016, ling2017, ling2018,
                                 ling2019, ling2020, ling2021, ling2022, ling2023,
                                 ling2024))
detach(package:plyr)

lc <- ling %>% 
  mutate(
    USER = case_when(
      USER == 'C' ~ 'Charter',
      USER == 'P' ~ 'Private',
      USER == '' ~ 'Unknown',
      TRUE ~ USER
    )
  )
# Annual lingcod samples by port and user per year
table(lc$USER, lc$PORT, lc$YEAR)


# Length data
length <- lc %>% 
  filter(
    !is.na(LENGTH)
  )


mean_length <- length %>% 
  group_by(YEAR, PORT, USER) %>% 
  reframe(
    MEANCM = mean(LENGTH, na.rm = TRUE),
    SECM = stderr(LENGTH)
  ) %>% 
  mutate(
    VARCM = SECM^2,
    RPCM = 1.96 * SECM / MEANCM
  )


# Age data 
age <- lc %>% 
  filter(
    !is.na(AGE),
    # Remove weird entries for age
    AGE != 'I',
    AGE != ''
  )

age$AGE <- as.numeric(age$AGE)

mean_age <- age %>% 
  group_by(YEAR, PORT, USER) %>% 
  reframe(
    MEANAGE = mean(AGE, na.rm = TRUE),
    SEAGE = stderr(AGE)
  ) %>% 
  mutate(
    VARAGE = SEAGE^2,
    RPAGE = 1.96 * SEAGE / MEANAGE
  )

# Sex composition
sex <- lc %>% 
  filter(
    SEX %in% c('M', 'F')
  ) %>% 
  mutate(
    SEX2 = case_when(
      SEX == 'M' ~ 0,
      SEX == 'F' ~ 1
    )
  ) %>% 
group_by(YEAR, PORT, USER) %>% 
reframe(
  Total = n(),
  Num_F = sum(SEX2),
  Pct_F = Num_F / Total * 100,
) 


# Looking at objectives

## Length
gt(
  mean_length %>% 
    select(YEAR, PORT, USER, MEANCM, SECM, VARCM, RPCM)
) %>% 
  tab_header(
    title = 'Lingcod Length',
    subtitle = 'RPCM is approx. relative precision on mean net wt (95% conf.)'
  )

## Age
gt(
  mean_age %>% 
    select(YEAR, PORT, USER, MEANAGE, SEAGE, VARAGE, RPAGE)
) %>% 
  tab_header(
    title = 'Lingcod Age',
    subtitle = 'RPAGE is approx. relative precision on mean net wt (95% conf.)'
  )


## Sex
gt(
  sex
) %>% 
  tab_header(
    title = 'Lingcod Sex Ratio (Percent Female)'
  )

