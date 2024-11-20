################################################################################
# This code queries interview data and provides total lingcod kept by port, year, and user
#
# Translated from SAS to R by CWM on 06/16/23
################################################################################

library(tidyverse)
library(readxl)

source("functions.R")

#Function call
get_data("data/Intervw/")


###

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021

int22$YEAR <- 2022

library(plyr)
int <- do.call(rbind.fill, list(intervw9219, int9204, int05, int06, int07, 
                                       int08, int09, int10, int11, int12, int13, 
                                       int14, int15, int16, int16prelim, int17, 
                                       int18, int19, int20, int21, int22))
detach(package:plyr)

#Add subareas
LCKEPT0322 <- int %>%
  mutate(
    STATAREA = case_when(
      YEAR >= 2021 ~ case_when(
        !is.na(ADFGSTATLING) ~ ADFGSTATLING,
        is.na(ADFGSTATLING) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
        is.na(ADFGSTATLING) & !is.na(ADFGSTATOTH) ~ ADFGSTATOTH
      ),
      TRUE ~ ADFGSTAT
    )
  ) %>% 
  area_split_sf() %>% 
  mutate(
    subset = case_when(
      SFmgmtarea == 'CI' & STATAREA %in% c(515908, 515934, 515935, 515933, 515932, 515931) ~ 'KBay',
      SFmgmtarea == 'CI' & STATAREA %in% c(515936, 515937, 525931, 525932, 515939, 515938, 516002, 526002, 526003, 516001) ~ 'UCI',
      SFmgmtarea == 'CI' & STATAREA %in% c(525902, 525901, 515907, 515906, 515905, 515903, 515901, 515902, 515904, 515831, 515832, 525837, 525936, 525835, 525831, 525836) ~ 'WGore',
      SFmgmtarea == 'CI' & STATAREA %in% c(525831, 525835, 525836, 525837, 515832, 515831) ~ 'Barrens',
      TRUE ~ 'Corner'
    ),
    subset = case_when(
      subset == '' ~ 'WCI',
      TRUE ~ subset  # To retain existing values for subset
    )
  ) %>%
  filter(YEAR >= 2003 & !is.na(STATAREA))  # Filter conditions for year and non-missing STATAREA

# Set default values for NA user
LCKEPT0322$USER[is.na(LCKEPT0322$USER)] <- 'Unknown'

# Drop rows with missing STATAREA
LCKEPT0322 <- LCKEPT0322[!is.na(LCKEPT0322$STATAREA), ]

# Sort the dataset
LCKEPT0322_sorted <- arrange(LCKEPT0322, USER, YEAR, subset)

# Calculate the sum of LCKEPT variable by user, year, and subset
CRAP <- LCKEPT0322_sorted %>%
  group_by(USER, YEAR, STATAREA) %>%
  summarize(LCKEPT = sum(LCKEPT, na.rm = TRUE))

# Calculate the sum of LCKEPT variable by user and year
TOT <- LCKEPT0322 %>%
  group_by(USER, YEAR) %>%
  summarize(TOTLC = sum(LCKEPT, na.rm = TRUE)) %>%
  ungroup()

# Merge CRAP and TOT datasets by user and year
CRAP2 <- merge(CRAP, TOT, by = c("USER", "YEAR"))

# Calculate P and SE variables
CRAP2 <- CRAP2 %>%
  mutate(P = LCKEPT / TOTLC,
         SE = sqrt((P * (1 - P)) / (TOTLC - 1)))

# Format P and SE variables
CRAP2$P <- format(CRAP2$P, nsmall = 3)
CRAP2$SE <- format(CRAP2$SE, nsmall = 3)

#Save the CRAP2 data set to folder so it can be merged with mapping data in the PWS_charter_lc_maps file
write.csv(CRAP2, file = "O:/DSF/GOAB/MAPS/GIS/lcharvestbystat0322.csv")


##################
#CI
##################
stat <- read_xlsx('O:/DSF/GOAB/MAPS/GIS/lingcod_harvest_by_port/PVG_2001_Alaska_Attributes_AlbersAreas.xlsx')
lcbystatarea <- CRAP2
#lcbystatarea <- read.csv("O:/DSF/GOAB/MAPS/GIS/lcharvestbystat0322.csv")

stats <- stat %>%
  mutate(STATAREA = STAT_AREA,
         deglong = as.integer(STATAREA / 10000),
         deglat = as.integer((STATAREA - (deglong * 10000)) / 100),
         Shape_Area_km2 = Shape_Area_m/1000) %>%
  filter(deglong >= 50 & deglong <= 54 & deglat >= 58 & deglat <= 60) %>%
  select(-STAT_AREA)


##Area selected contains 85 stat areas
##Need to make a data set consisting of a list of all stat areas in teh defined
##region that repeats for each YEAR 2003-2016
# Create a new data frame "list" from the existing "stats" data frame
list <- stats %>%
  select(STATAREA, Shape_Area_km2)


# Create a new data frame "statlist" to store the repeated list for each YEAR
statlist <- data.frame()
YEARS <- 2003:2018

for (yr in YEARS) {
  temp_df <- list %>%
    mutate(YEAR = yr)
  statlist <- bind_rows(statlist, temp_df)
}

# Print the count of stat areas for each YEAR
freq_table <- statlist %>%
  group_by(YEAR) %>%
  summarise(count = n())

##Now merge harvest and spatial data
# Sort the "lcbystatarea" data frame by YEAR and STATAREA
lcbystatarea <- lcbystatarea %>%
  arrange(YEAR, STATAREA)

# Sort the "statlist" data frame by YEAR and STATAREA
statlist <- statlist %>%
  arrange(YEAR, STATAREA)

# Merge the "lcbystatarea" and "statlist" data frames by YEAR and STATAREA
lcbystatarea <- merge(lcbystatarea, statlist, by = c("YEAR", "STATAREA"))

# Format the column P to have 4 digits with 2 decimal places
lcbystatarea$P <- format(lcbystatarea$P, nsmall = 2)
