################################################################################
# This code queries interview data and provides total lingcod kept by port, year, and user
#
# Translated from SAS to R by CWM on 06/16/23
################################################################################

library(tidyverse)


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

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021

int22$YEAR <- 2022

library(plyr)
LCKEPT0322 <- do.call(rbind.fill, list(int05, int06, int07, int08, int09, int10,
                                     int11, int12, int13, int14, int15, int16, 
                                     int16prelim, int17, int18, int19, int20, 
                                     int21, int22, int9204, intervw9219 ))
detach(package:plyr)

#Add subareas
LCKEPT0322 <- LCKEPT0322 %>%
  mutate(
    SFmgmtarea = case_when(
      ADFGSTAT > 440000 & ADFGSTAT < 480000 |
        ADFGSTAT %in% c(485430, 485500, 485530, 485600, 485630, 485700, 485730, 485800, 485831, 485901, 485931, 485932, 485935, 486001, 486002, 486003, 486004, 486005, 486031, 486032, 486033, 486034, 486100) ~ 'PWS',
      ADFGSTAT %in% c(485832, 485902, 485933, 485934, 485935, 486002, 495831, 495901, 495902, 495931, 495932, 495933, 495934, 495935, 495936, 495937, 495938, 495939, 496001, 496002, 505831, 505901, 505902, 505903, 505904, 505905, 505906, 505907, 505908, 505909, 505931, 505932, 505933, 505934) ~ 'NG',
      ADFGSTAT %in% c(495800, 495832, 505700, 505730, 505800, 505832, 515630, 515700, 515730, 515801, 515802, 515833, 525600, 525630, 525701, 525702, 525703, 525731, 525732, 525733, 525801, 525802, 525803, 525804, 525805, 525806, 525807, 525832, 525833, 525834, 535601, 535602, 535631, 535632, 535633, 535634, 535701, 535702, 535703, 535704, 535705, 535706, 535707, 535731, 535732, 535733, 535734, 535802, 535803, 535831, 545601, 545602, 545631, 545632, 545633, 545701, 545702, 545703, 545704, 545732, 545733, 545734, 545804, 555630, 555701, 555733) ~ 'Kod',
      ADFGSTAT %in% c(555731, 555732, 545731, 545801, 545802, 545803, 535801, 535832) ~ 'AKPen',
      ADFGSTAT %in% c(515831, 515832, 515901, 515902, 515903, 515904, 515905, 515906, 515907, 515908, 515931, 515932, 515933, 515934, 515935, 515936, 515937, 515938, 515939, 516001, 516002, 525831, 525835, 525836, 525837, 525901, 525902, 525931, 525932, 526002, 526003, 535833, 535834, 535901, 535902, 535903, 535904, 535905, 535906, 535931, 535932, 535933, 545900) ~ 'CI',
      TRUE ~ SFmgmtarea  # To retain existing values for SFmgmtarea
    ),
    SFmgmtarea = case_when(
      is.na(ADFGSTAT) & PORT == 'Homer' ~ 'CI',
      is.na(ADFGSTAT) & PORT == 'Kodiak' ~ 'Kod',
      is.na(ADFGSTAT) & PORT == 'Whittier' ~ 'PWS',
      is.na(ADFGSTAT) & PORT == 'Valdez' ~ 'PWS',
      TRUE ~ SFmgmtarea  # To retain existing values for SFmgmtarea
    ),
    subset = case_when(
      SFmgmtarea == 'CI' & ADFGSTAT %in% c(515908, 515934, 515935, 515933, 515932, 515931) ~ 'KBay',
      SFmgmtarea == 'CI' & ADFGSTAT %in% c(515936, 515937, 525931, 525932, 515939, 515938, 516002, 526002, 526003, 516001) ~ 'UCI',
      SFmgmtarea == 'CI' & ADFGSTAT %in% c(525902, 525901, 515907, 515906, 515905, 515903, 515901, 515902, 515904, 515831, 515832, 525837, 525936, 525835, 525831, 525836) ~ 'WGore',
      SFmgmtarea == 'CI' & ADFGSTAT %in% c(525831, 525835, 525836, 525837, 515832, 515831) ~ 'Barrens',
      TRUE ~ 'Corner'
    ),
    subset = case_when(
      subset == '' ~ 'WCI',
      TRUE ~ subset  # To retain existing values for subset
    )
  ) %>%
  filter(YEAR >= 2003 & !is.na(ADFGSTAT))  # Filter conditions for year and non-missing ADFGSTAT

# Set default values for NA user
LCKEPT0322$USER[is.na(LCKEPT0322$USER)] <- 'Unknown'

# Drop rows with missing ADFGSTAT
LCKEPT0322 <- LCKEPT0322[!is.na(LCKEPT0322$ADFGSTAT), ]

# Sort the dataset
LCKEPT0322_sorted <- arrange(LCKEPT0322, USER, YEAR, subset)

# Calculate the sum of LCKEPT variable by user, year, and subset
CRAP <- LCKEPT0322_sorted %>%
  group_by(USER, YEAR, subset) %>%
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
library(readxl)
stats <- read_xlsx('O:/DSF/GOAB/MAPS/GIS/lingcod_harvest_by_port/PVG_2001_Alaska_Attributes_AlbersAreas.xlsx')
lcbystatarea <- CRAP2
#lcstatbyarea <- read.csv("O:/DSF/GOAB/MAPS/GIS/lcharvestbystat0322.csv")

stats <- stats %>%
  mutate(ADFGSTAT = STAT_AREA,
         deglong = as.integer(ADFGSTAT / 10000),
         deglat = as.integer((ADFGSTAT - (deglong * 10000)) / 100),
         Shape_Area_km2 = Shape_Area_m/1000) %>%
  filter(deglong >= 50 & deglong <= 54 & deglat >= 58 & deglat <= 60) %>%
  select(-STAT_AREA)


##Area selected contains 85 stat areas
##Need to make a data set consisting of a list of all stat areas in teh defined
##region that repeats for each YEAR 2003-2016
# Create a new data frame "list" from the existing "stats" data frame
list <- stats %>%
  select(ADFGSTAT, Shape_Area_km2)


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
# Sort the "lcbystatarea" data frame by YEAR and adfgstat
lcbystatarea <- lcbystatarea %>%
  arrange(YEAR, ADFGSTAT)

# Sort the "statlist" data frame by YEAR and adfgstat
statlist <- statlist %>%
  arrange(YEAR, ADFGSTAT)

# Merge the "lcbystatarea" and "statlist" data frames by YEAR and adfgstat
lcbystatarea <- merge(lcbystatarea, statlist, by = c("YEAR", "ADFGSTAT"))

# Format the column P to have 4 digits with 2 decimal places
lcbystatarea$P <- format(lcbystatarea$P, nsmall = 2)