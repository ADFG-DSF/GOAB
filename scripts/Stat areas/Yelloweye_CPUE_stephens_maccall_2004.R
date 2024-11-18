################################################################################
# Looking at Yelloweye CPUE, attempting to replicated the methods used
# for Boccacio in Stephens and MacCall 2004
################################################################################
library(tidyverse)
library(readxl)

################################################################################
####################### LOAD DATA ##############################################
################################################################################
get_data <- function(a) {
  
  
  print(a)
  
  files <- list.files(path=a,
                      pattern="*.csv", full.names=F, recursive=FALSE)
  
  
  for(i in seq(1, length(files))) {
    print(files[[i]])
    n <- gsub(".csv", "",files[[i]])
    assign(n, read.csv(paste0(a,files[[i]])), envir = .GlobalEnv)
    
  }
  
}

#Function call
get_data("O:/DSF/GOAB/R data/Intervw/")


###
int_data <- data.frame()  # create empty data frame for int_data

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021 #matching ling areas to work with targetted ling areas from previous rears

int22$YEAR <- 2022

int23$YEAR <- 2023


library(plyr)
int <- do.call(rbind.fill, list(int05, int06, int07, int08, int09, int10,
                                int11, int12, int13, int14, int15, int16, 
                                int16prelim, int17, int18, int19, int20, 
                                int21, int22, int23, int9204, intervw9219 ))
detach(package:plyr)


int_all <- int %>% 
  mutate(
    TARGET = case_when(
      is.na(TARGET) & !is.na(Target) ~ Target
    )
  ) %>% 
  select(-X, -P_KPT_CAS, -Target)
################################################################################

# Which areas have had more than 10 Yelloweye caught - indicates Yelloweye habitat
ye_10_plus <- int_all %>% 
  # filter(YEAR >= 2011) %>% #Yelloweye catch started being recorded in interviews in 2011
  mutate(
    STAT_AREA = case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      is.na(ADFGSTATRF) & !is.na(STATAREA_BOT) ~ STATAREA_BOT,
      is.na(ADFGSTATRF) & !is.na(ADFGSTAT) ~ ADFGSTAT
  ),
  YE_CAUGHT = YEKEPT + YERSURF + YERDRM
  ) %>% 
  group_by(STAT_AREA) %>% 
  summarise(
    tot_YE = sum(YE_CAUGHT, na.rm = TRUE)
  ) %>% 
  filter(tot_YE >= 10)

# Only keep interviews in the identified Yelloweye habitat
int_ye_10_plus <- int_all %>%
  filter(YEAR >= 2011) %>% #Yelloweye catch started being recorded in interviews in 2011
  mutate(
    STAT_AREA = case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      is.na(ADFGSTATRF) & !is.na(STATAREA_BOT) ~ STATAREA_BOT,
      is.na(ADFGSTATRF) & !is.na(ADFGSTAT) ~ ADFGSTAT,
      TRUE ~ 0
    ),
    YE_CAUGHT = YEKEPT + YERSURF + YERDRM,
    YE_HOURS_Both = case_when(
      HOURS_R == 0 & HOURS_COMBI!= 0 ~ HOURS_COMBI,
      !is.na(BOATHRS) ~ BOATHRS,
      TRUE ~ HOUR_TOT
    ),
    # Number of Hours fished X Number of Anglerdays (divide by total days as that is factored in to angler days)
    TOT_HOURS = (ANGLDAYS * YE_HOURS_Both)/NUMDAYS,
    # Fish caught per hour per angler
    YE_CPUE = YE_CAUGHT / TOT_HOURS) %>% 
  filter(STAT_AREA %in% ye_10_plus$STAT_AREA, !is.na(YE_CAUGHT))


int_ye_10_plus_charter <- int_ye_10_plus %>% 
  filter(USER == 'Charter') # Stephens and MacCall prefer charter to privates as more representative of site species composition

model_dat <- int_ye_10_plus_charter %>% filter(!is.na(YE_CPUE), YE_CPUE < 10000, YE_CPUE > 0)

model_dat_w_zero <- int_ye_10_plus_charter %>% filter(!is.na(YE_CPUE), YE_CPUE < 10000)

model_dat2 <- int_ye_10_plus %>% filter(!is.na(YE_CPUE), YE_CPUE < 10000, YE_CPUE > 0)
# Fit the GLM model
glm_model <- glm(log(YE_CPUE) ~ YEAR + STAT_AREA, data = model_dat, family = gaussian) 
    # AIC: 11604
# Print the summary of the model
summary(glm_model) # Both Stat Area and Year have a significant effect on Yelloweye CPUE - Increasing CPUE over time

# Including zeros, so no log()
glm_model_zero <- glm(YE_CPUE ~ YEAR + STAT_AREA, data = model_dat_w_zero, family = gaussian)

summary(glm_model_zero)
 # AIC: -2435 -  best so far

#

glm_model2 <- glm(log(YE_CPUE) ~ YEAR + STAT_AREA + USER, data = model_dat2, family = gaussian)

summary(glm_model2)
    # AIC: 25035 - worse than not factoring in USER in the charter only dataset

glm_model3 <- glm(log(YE_CPUE) ~ YEAR + STAT_AREA, data = model_dat2, family = gaussian)

summary(glm_model3)
    # AIC: 25275 ~ better to factor in user group, not as descriptive as Charter only

# Using just the year with data subsetted for common YE catch, second model in paper
glm_model4 <- glm(log(YE_CPUE) ~ YEAR, data = model_dat, family = gaussian)

summary(glm_model4)
    # AIC: 25330, worst so far, definitely worth factoring in location

# Testing a binary model of presense vs. absense
binary_model_dat <- model_dat_w_zero %>% 
  mutate(
    YE_CPUE = case_when(
      YE_CPUE == 0 ~ 0,
      YE_CPUE > 0 ~ 1
    )
  )

glm_model5 <- glm(YE_CPUE ~ YEAR + STAT_AREA, data = binary_model_dat, family = binomial)

summary(glm_model5) # Year still significant (but less so) with binary CPUE. Stat area still significant

    # AIC: 14819

## Looking at trends in CPUE in areas with more than 10 YE caught as possible indicator of abundance
plot_data <- model_dat_w_zero %>% 
  group_by(YEAR, STAT_AREA) %>% 
  summarize(avg_YE_CPUE = mean(YE_CPUE, na.rm = TRUE))


ggplot(data = plot_data, aes(x = YEAR, y = avg_YE_CPUE)) +
  geom_line() +
  ylim(0, 0.7) +
  facet_wrap(~STAT_AREA)
###################
# Working with logbook data
logbook <- read_xlsx("O:/DSF/SportF/Data/Charter_logbook/Copy of 2006 - 2021 Final Saltwater Summaries.xlsx",
                     #sheet = "Bottomfish by SWHS Area", 
                     range = "Bottomfish by SWHS Area!A6:AB6732") %>% filter(Year > 0)

ye_10_plus_l <- logbook %>% 
  mutate(
    YE_CAUGHT = `Yelloweye Kept` + `Yelloweye Released`,
    STAT_AREA = `Bottomfish Stat Area`
  ) %>% 
  group_by(STAT_AREA) %>% 
    summarise(
      tot_YE = sum(YE_CAUGHT, na.rm = TRUE)
    ) %>% 
    filter(tot_YE >= 10)

logbook_ye_10_plus <- logbook %>% 
  mutate(
    YE_CAUGHT = `Yelloweye Kept` + `Yelloweye Released`,
    STAT_AREA = `Bottomfish Stat Area`
  ) %>% 
  filter(STAT_AREA %in% ye_10_plus_l$STAT_AREA)

###################