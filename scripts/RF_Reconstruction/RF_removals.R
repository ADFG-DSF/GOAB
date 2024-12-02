################################################################################
# Rockfish Harvest Reconstruction
#
# Estimated removals in lbs for Black and Yelloweye Rockfish
# Using harvest and release estimates from Howard et al./ Sara W.
#
# Moved to R from the methods used in Excel and SAS previously by CWM in 09/2023
#
#
# Updated with Upper and lower 95 intervals 
# calculated using harvest/release estimate variance 02/14/23 CWM
#
# Updated through 2023 10/03/2024 CWM
################################################################################

library(readxl)
library(tidyverse)
source("functions.R")
################################################################################
#################################### Input All Data ############################
################################################################################

################################## Harvest and Release #########################
# Black Rockfish Harvest and Release

## Load in BRF harvest data
brf_harv <- read_excel(path = "data/RF_Harvest_Reconstruction/harvest_estimates_Howard_thru2023.xlsx",
                       sheet = "BRF harvest", range = "A1:Z400")[, c(2:3, 8:9, 17:18, 22:23)]
## Rename columns
colnames(brf_harv) <- c("Year", "CFMU", "Guided_Harvest_BRF", "VarGuided_Harvest_BRF", "Unguided_Harvest_BRF", "VarUnguided_Harvest_BRF", 
                        "Total_Harvestv", "VarTotal_Harvest_BRF")

## Pivot the data to be sorted by User for Harvest
brf_harv_user <- brf_harv %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(Guided_Harvest_BRF, Unguided_Harvest_BRF),
               names_to = "USER",
               values_to = "Harvest") %>%
  mutate(USER = ifelse(USER == "Guided_Harvest_BRF", "Charter", "Private"),
         Assemblage = "Black") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "Harvest")

## Pivot the data to be sorted by User for Harvest Variance
brf_var_harv <- brf_harv %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(VarGuided_Harvest_BRF, VarUnguided_Harvest_BRF),
               names_to = "USER",
               values_to = "VarHarvest") %>%
  mutate(USER = ifelse(USER == "VarGuided_Harvest_BRF", "Charter", "Private"),
         Assemblage = "Black") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "VarHarvest")
## Combine harvest and variance
brf_harv_long <- merge(brf_harv_user, brf_var_harv, by = c("CFMU", "Year", "USER", "Assemblage"))


## Load in BRF Release data
brf_rel <- read_excel(path = "data/RF_Harvest_Reconstruction/release_estimates_Howard_thru2023.xlsx",
                      sheet = "BRF release", range = "A1:Z400")[, c(2:3, 8:9, 17:18, 22:23)]

colnames(brf_rel) <- c("Year", "CFMU", "Guided_Release_BRF", "VarGuided_Release_BRF", "Unguided_Release_BRF", "VarUnguided_Release_BRF", 
                       "Total_Release_BRF", "VarTotal_Release_BRF")

## Pivot the data to be sorted by user for Release
brf_rel_user <- brf_rel %>%
 filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(Guided_Release_BRF, Unguided_Release_BRF),
               names_to = "USER",
               values_to = "Release") %>%
  mutate(USER = ifelse(USER == "Guided_Release_BRF", "Charter", "Private"),
         Assemblage = "Black") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "Release")

## Pivot the data to be sorted by user for Release Variance
brf_var_rel <- brf_rel %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(VarGuided_Release_BRF, VarUnguided_Release_BRF),
               names_to = "USER",
               values_to = "VarRelease") %>%
  mutate(USER = ifelse(USER == "VarGuided_Release_BRF", "Charter", "Private"),
         Assemblage = "Black") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "VarRelease")

## Combine Release and Variance
brf_rel_long <- merge(brf_rel_user, brf_var_rel, by = c("CFMU", "Year", "USER", "Assemblage"))


# Yelloweye Rockfish Harvest and Release
## Load in YE harvest data
ye_harv <- read_excel(path = "data/RF_Harvest_Reconstruction/harvest_estimates_Howard_thru2023.xlsx",
                      sheet = "YE harvest", range = "A1:Z400")[, c(2:3, 9:10, 20:21, 25:26)] 
## Rename columns
colnames(ye_harv) <- c("Year", "CFMU", "Guided_Harvest_YE", "VarGuided_Harvest_YE", "Unguided_Harvest_YE", "VarUnguided_Harvest_YE", 
                       "Total_Harvest_YE", "VarTotal_Harvest_YE")

## Pivot the data to be sorted by User for Harvest
ye_harv_user <- ye_harv %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(Guided_Harvest_YE, Unguided_Harvest_YE),
               names_to = "USER",
               values_to = "Harvest") %>%
  mutate(USER = ifelse(USER == "Guided_Harvest_YE", "Charter", "Private"),
         Assemblage = "Yelloweye") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "Harvest")

## Pivot the data to be sorted by User for Harvest Variance
ye_var_harv <- ye_harv %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(VarGuided_Harvest_YE, VarUnguided_Harvest_YE),
               names_to = "USER",
               values_to = "VarHarvest") %>%
  mutate(USER = ifelse(USER == "VarGuided_Harvest_YE", "Charter", "Private"),
         Assemblage = "Yelloweye") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "VarHarvest")

## Combine Harvest and Variance
ye_harv_long <- merge(ye_harv_user, ye_var_harv, by = c("CFMU", "Year", "USER", "Assemblage"))

## Load in YE Release data
ye_rel <- read_excel(path = "data/RF_Harvest_Reconstruction/release_estimates_Howard_thru2023.xlsx",
                     sheet = "YE release", range = "A1:Z400")[, c(2:3, 9:10, 20:21, 25:26)] 
## Rename Columns
colnames(ye_rel) <- c("Year", "CFMU", "Guided_Release_YE", "VarGuided_Release_YE", "Unguided_Release_YE", "VarUnguided_Release_YE", 
                      "Total_Release_YE", "VarTotal_Release_YE")

## Pivot the data to be sorted by User for Release
ye_rel_user <- ye_rel %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(Guided_Release_YE, Unguided_Release_YE),
               names_to = "USER",
               values_to = "Release") %>%
  mutate(USER = ifelse(USER == "Guided_Release_YE", "Charter", "Private"),
         Assemblage = "Yelloweye") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "Release")

## Pivot the data to be sorted by User for Release Variance
ye_var_rel <- ye_rel %>%
  filter(!is.na(CFMU)) %>%
  pivot_longer(cols = c(VarGuided_Release_YE, VarUnguided_Release_YE),
               names_to = "USER",
               values_to = "VarRelease") %>%
  mutate(USER = ifelse(USER == "VarGuided_Release_YE", "Charter", "Private"),
         Assemblage = "Yelloweye") %>% 
  select("CFMU", "Year", "USER", "Assemblage", "VarRelease")

## Combine Releae and Variance
ye_rel_long <- merge(ye_rel_user, ye_var_rel, by = c("CFMU", "Year", "USER", "Assemblage"))

#### Combine data into Harvest and Release
harv <- bind_rows(brf_harv_long, ye_harv_long)

rel <- bind_rows(brf_rel_long, ye_rel_long)

########################## Table 1 #############################################
## Combine Harvest and Release Data for both species
table1 <- merge(rel, harv, by = c("CFMU", "Year", "USER", "Assemblage")) %>% 
  mutate(CFMU = case_when(CFMU == "NORTHEAS" ~ "NORTHEAST",
                          TRUE ~ CFMU))


####################### Port Sampling Data #####################################

get_data("data/Intervw/") # Load in Interview Data
get_data("data/RF/") # Load in Rockfish Bio Data

# Add Year to data missing this variable
int21$YEAR <- 2021 



rock2020$LENGTH <- rock2020$FORK_LENGTH # Starting in 2020, Fork length was collected

library(plyr)
# Combine interview data
intvw <- do.call(rbind.fill, list(int9204, int05, int06, int07, int08,
                                  int09, int10, int11, int12, int13, int14,
                                  int15, int16, int17, int18, int19, int20,
                                  int21, int22, int23))
# Combine Rockfish data
rfdat <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                  rock2004, rock2005, rock2006, rock2007, rock2008, 
                                  rock2009, rock2010, rock2011, rock2012, rock2013, 
                                  rock2014, rock2015, rock2016, rock2017, rock2018, 
                                  rock2019, rock2020, rock2021, rock2022, rock2023)) 

detach(package:plyr)

# Use the area split function to assign CFMU to data
int <- intvw %>% mutate(
  STATAREA = case_when(
    !is.na(ADFGSTATRF) ~ ADFGSTATRF,
    is.na(ADFGSTATRF) & !is.na(STATAREA_BOT) ~ STATAREA_BOT,
    #is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
    TRUE ~ ADFGSTAT
  )) %>%
  filter(YEAR >= 1998) %>% 
  area_split_cfmu() %>% 
  filter(CFMU %in% c("NORTHEAST", "CI", "NG",
                     # "PWS"
                     "PWSI", "PWSO")
  )
  

rf <- rfdat %>% filter(SP %in% c(142,145)) %>%
  filter(YEAR >= 1998) %>% 
  area_split_cfmu() %>% 
  filter(CFMU %in% c("NORTHEAST", "CI", "NG",
                     # "PWS"
                     "PWSI", "PWSO")
  )

############################# Mortality Rates ##################################
# Mortality Rates compiled by Adam St. Saviour and Brottany Blain, from peer reviewed literature sources
# Yelloweye using Non-Black release mortalities
# Depth Categories:
  # 1: 0-59'
  # 2: 60-119'
  # 3: 120-179'
  # 4: 180'+

release_mort <- data.frame(
  Assemblage = c("Black", "Black", "Black", "Black",
                 "Black", "Black", "Black", "Black",
                 "Yelloweye", "Yelloweye", "Yelloweye", "Yelloweye",
                 "Yelloweye", "Yelloweye", "Yelloweye", "Yelloweye"),
  Release_Method = c("Surface", "Surface", "Surface", "Surface",
                     "DRM", "DRM", "DRM", "DRM",
                     "Surface", "Surface", "Surface", "Surface",
                     "DRM", "DRM", "DRM", "DRM"),
  RelCat = c(1 , 2, 3, 4,
             1 , 2, 3, 4,
             1 , 2, 3, 4,
             1 , 2, 3, 4),
  Mortality = c(0.00, 0.12, 0.35, 0.80,
                0.00, 0.04, 0.31, 0.40,
                0.00, 0.70, 0.94, 1.0,
                0.01, 0.01, 0.01, 0.20)
)

################################################################################
######################### Data Analysis ########################################
################################################################################

########################## Table 2 ############################################
## Release by Depth Category
int1 <- int %>%
  mutate(
    # Release Categories 1-4. 
    # Coding release catergories will allow creation of a column for number of fish released by category
    PelRelCat = case_when(
      PELDEPTH >= 1 & PELDEPTH <= 59 ~ 1, # Category 1 depth between 1 and 59
      PELDEPTH >= 60 & PELDEPTH <= 119 ~ 2, # Category 2 depth between 60 and 119
      PELDEPTH >= 120 & PELDEPTH <= 179 ~ 3, # Category 3 depth between 120 and 179
      PELDEPTH >= 180 ~ 4, # Catergory 4 depth 180+
      TRUE ~ NA_real_
    ),
    YERelCat = case_when(
      YEDEPTH >= 1 & YEDEPTH <= 59 ~ 1,
      YEDEPTH >= 60 & YEDEPTH <= 119 ~ 2,
      YEDEPTH >= 120 & YEDEPTH <= 179 ~ 3,
      YEDEPTH >= 180 ~ 4,
      TRUE ~ NA_real_
    ),
    NPRelCat = case_when(
      NPDEPTH >= 1 & NPDEPTH <= 59 ~ 1,
      NPDEPTH >= 60 & NPDEPTH <= 119 ~ 2,
      NPDEPTH >= 120 & NPDEPTH <= 179 ~ 3,
      NPDEPTH >= 180 ~ 4,
      TRUE ~ NA_real_
    )) %>%
  filter(YEAR >= 2013, # No information on method of release or depth of capture prior to 2013
         TRFREL != 0) # Don't care about trips where no RF released

#### Sample Sizes are probably too small to break these proportions up by year, pool all years for now

####################### Black ##################################
# Curently, pelagic rockfish data is used as a proxy for black rockfish
# Port sampling interview data does not split up pelagic rockfish catch by species, but black rockfish are by far the most predomninat species

# Pelagic rockfish release data
pelcat <- int1 %>% 
  filter(!is.na(PelRelCat))


# Calculate means for 'pelrsurf' and 'pelrdrm' by 'CFMU' and 'user'
means_pelcat <- pelcat %>%
  group_by(CFMU, USER) %>%
  summarise(
    totpelrsurf = sum(PELRSURF),
    totpelrdrm = sum(PELRDRM)
  )


# Calculate means for 'pelrsurf' and 'pelrdrm' by 'CFMU', 'user', and 'pelrelcat'
pelrel <- pelcat %>%
  group_by(CFMU, USER, PelRelCat) %>%
  summarise(
    N_trips = n(),
    pelrsurf = sum(PELRSURF),
    pelrdrm = sum(PELRDRM)
  )

# Merge 'means_pelcat' and 'pelrel' data frames by 'CFMU' and 'user'
# Final Pelagic Rockfish Release data
pelcat1 <- inner_join(means_pelcat, pelrel, by = c("CFMU", "USER")) %>%
  mutate(
    Pcatsurf = pelrsurf / totpelrsurf,
    Pcatdrm = pelrdrm / totpelrdrm,
    pelrtot = pelrsurf + pelrdrm
  ) %>% 
  group_by(CFMU, USER) %>% 
  mutate(
    P_Rel = pelrtot / sum(pelrtot)
  ) %>% 
  select(CFMU, USER, N_trips, PelRelCat, pelrsurf, pelrdrm, pelrtot, P_Rel, Pcatsurf, Pcatdrm)


####################### Yelloweye ##################################

# Yelloweye rockfish release data
yecat <- int1 %>% 
  filter(!is.na(YERelCat))

# Calculate means for 'yersurf' and 'yerdrm' by 'CFMU' and 'user'
means_yecat <- yecat %>%
  group_by(CFMU, USER) %>%
  summarise(
    totyersurf = sum(YERSURF),
    totyerdrm = sum(YERDRM)
  )


# Calculate means for 'yersurf' and 'yerdrm' by 'CFMU', 'user', and 'pelrelcat'
yerel <- yecat %>%
  group_by(CFMU, USER, YERelCat) %>%
  summarise(
    N_trips = n(),
    yersurf = sum(YERSURF),
    yerdrm = sum(YERDRM)
  )

# Merge 'meanse_yecat' and 'yerel' data frames by 'CFMU' and 'user'
yecat1 <- inner_join(means_yecat, yerel, by = c("CFMU", "USER")) %>%
  mutate(
    Pcatsurf = yersurf / totyersurf,
    Pcatdrm = yerdrm / totyerdrm,
    yertot = yersurf + yerdrm
  ) %>%
  group_by(CFMU, USER) %>%
  mutate(
    P_Rel = yertot / sum(yertot)
  ) %>% 
  select(CFMU, USER, N_trips, YERelCat, yersurf, yerdrm, yertot, P_Rel, Pcatsurf, Pcatdrm)


# Calculate overall mean release percentages to use in areas with a low response rate
ye_prop_mean <- yecat1 %>% 
  filter(CFMU %in% c("NG", "PWSI", "PWSO")) %>% 
  group_by(USER, YERelCat) %>% 
  summarise(
    P_surf_Rel_mean = mean(Pcatsurf, na.rm = TRUE),
    P_DRM_Rel_mean = mean(Pcatdrm, na.rm = TRUE),
    P_Rel_mean = mean(P_Rel, na.rm = TRUE)
  )

ye_prop_mean2 <- ye_prop_mean %>% 
  rename(RelCat = YERelCat) %>% 
  mutate(
    Assemblage = "Yelloweye"
  )
# Overall mean for Release percentages used when low response rate
yecat1 <- yecat1 %>% 
  mutate(
    Pcatsurf = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 1 ~ as.numeric(ye_prop_mean[1,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 2 ~ as.numeric(ye_prop_mean[2,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 3 ~ as.numeric(ye_prop_mean[3,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 4 ~ as.numeric(ye_prop_mean[4,3]),
      
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 1 ~ as.numeric(ye_prop_mean[5,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 2 ~ as.numeric(ye_prop_mean[6,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 3 ~ as.numeric(ye_prop_mean[7,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 4 ~ as.numeric(ye_prop_mean[8,3]),
      TRUE ~ Pcatsurf
    ),
    Pcatdrm = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 1 ~ as.numeric(ye_prop_mean[1,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 2 ~ as.numeric(ye_prop_mean[2,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 3 ~ as.numeric(ye_prop_mean[3,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 4 ~ as.numeric(ye_prop_mean[4,4]),
      
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 1 ~ as.numeric(ye_prop_mean[5,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 2 ~ as.numeric(ye_prop_mean[6,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 3 ~ as.numeric(ye_prop_mean[7,4]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 4 ~ as.numeric(ye_prop_mean[8,4]),
      TRUE ~ Pcatdrm
    ),
    P_Rel = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 1 ~ as.numeric(ye_prop_mean[1,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 2 ~ as.numeric(ye_prop_mean[2,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 3 ~ as.numeric(ye_prop_mean[3,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & YERelCat == 4 ~ as.numeric(ye_prop_mean[4,5]),
      
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 1 ~ as.numeric(ye_prop_mean[5,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 2 ~ as.numeric(ye_prop_mean[6,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 3 ~ as.numeric(ye_prop_mean[7,5]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & YERelCat == 4 ~ as.numeric(ye_prop_mean[8,5]),
      TRUE ~ P_Rel
    )
  )


# Rename columns for pelagic and yelloweye datasets so they can be merged
pelcat2 <- pelcat1 %>% 
  mutate(
    Assemblage = "Black",
  ) %>% 
  rename(
    RelCat = PelRelCat,
    Rsurf = pelrsurf,
    Rdrm = pelrdrm,
    Rtot = pelrtot
  )

yecat2 <- yecat1 %>% 
  mutate(
    Assemblage = "Yelloweye",
  ) %>% 
  rename(
    RelCat = YERelCat,
    Rsurf = yersurf,
    Rdrm = yerdrm,
    Rtot = yertot
  )

############# Create Table 2 #####################
# merge pelagic and yelloweye datasets
table2 <- rbind(pelcat2, yecat2) %>%  
  select("CFMU", "USER", "Assemblage", "N_trips", "RelCat", 
         "Rsurf", "Rdrm", "Rtot", "P_Rel", "Pcatsurf", "Pcatdrm")


######################## Table 3 ###############################################
## Release mortality


table3.1 <- table2 %>%
  inner_join(release_mort, by = c("RelCat", "Assemblage"), relationship = "many-to-many") %>% # Release moralities by gear type and depth from literature
  left_join(ye_prop_mean2, by = c("USER", "RelCat", "Assemblage")) %>% # Overall release mortality means for Yelloweye
  # Calculating mortality rates for low response yelloweye
  mutate(Mort_rate = case_when(
    CFMU %in% c("CI", "NORTHEAST") & Assemblage == "Yelloweye" ~ P_Rel_mean * Mortality,
    TRUE ~ case_when(
      Release_Method == "Surface" ~ Pcatsurf * Mortality,
      Release_Method == "DRM" ~ Pcatdrm * Mortality,
      TRUE ~ 0  # Handle other cases (if any) with a value of 0 
    )
  ),
  # Apportioned mortality rate pre 2013. Takes into account depth of capture categories (2013-2019 data), but assumes all fish released at surface.
  Mort_pre_13 = case_when(
    Release_Method == "Surface" ~ P_Rel * Mortality, 
    TRUE ~ 0
  ) ) %>%
  group_by(CFMU, USER, Assemblage) %>%
  mutate(
    Mort_pre_13 = sum(Mort_pre_13)
    ) %>% 
  group_by(CFMU, USER, Assemblage, Release_Method) %>%
  reframe(Mort_rateBik = sum(Mort_rate),
            P_rel_surf_v_DRM = case_when(
              Release_Method == "Surface" ~ sum(Rsurf) / sum(Rtot),
              Release_Method == "DRM" ~ 1 - (sum(Rsurf) / sum(Rtot))
            ),
          Mort_pre_13
            ) %>% 
  distinct() 
## Mean YE prop. release surf vs. DRM for low sample sizes (CI, NE)
Mean_ye_prop_S_drm <- table3.1 %>% 
  filter(CFMU %in% c("NG", "PWSI", "PWSO"), Assemblage == "Yelloweye") %>% 
  group_by(USER, Release_Method) %>% 
  summarise(prop = mean(P_rel_surf_v_DRM))

## Mortality for low sample sizes (CI, NE)
pre_13_mort <- ye_prop_mean %>%
  rename(
    RelCat = YERelCat
  ) %>% 
  inner_join(release_mort, by = c("RelCat"), relationship = "many-to-many") %>% 
  mutate(
    Mort_pre_13 = case_when(
      Release_Method == "Surface" ~ P_Rel_mean * Mortality, 
      TRUE ~ 0
    )
  ) %>% 
  group_by(USER, Assemblage) %>% 
  summarise(Mort_pre_13 = sum(Mort_pre_13))
## MortBik for low sample size YE

ye_bik <- ye_prop_mean %>%
  rename(
    RelCat = YERelCat
  ) %>% 
  inner_join(release_mort, by = c("RelCat"), relationship = "many-to-many") %>% 
  filter(Assemblage == "Yelloweye") %>% 
  mutate(
    mort_rateBik = case_when(
      Release_Method == "Surface" ~ Mortality * P_surf_Rel_mean,
      Release_Method == "DRM" ~ Mortality * P_DRM_Rel_mean
    ) 
  ) %>% 
    group_by(USER, Release_Method) %>% 
    summarise(mort_rateBik = sum(mort_rateBik))


###############
# Mortality rates for low response yelloweye
table3.2 <- table3.1 %>% 
  mutate(
    P_rel_surf_v_DRM = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & Release_Method == "DRM" & Assemblage == "Yelloweye" ~ as.numeric(Mean_ye_prop_S_drm[1,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & Release_Method == "Surface" & Assemblage == "Yelloweye" ~ as.numeric(Mean_ye_prop_S_drm[2,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & Release_Method == "DRM" & Assemblage == "Yelloweye" ~ as.numeric(Mean_ye_prop_S_drm[3,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & Release_Method == "Surface" & Assemblage == "Yelloweye" ~ as.numeric(Mean_ye_prop_S_drm[4,3]),
      TRUE ~ P_rel_surf_v_DRM
    ),
    Mort_pre_13 = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & Assemblage == "Yelloweye" ~ as.numeric(pre_13_mort[2,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & Assemblage == "Yelloweye" ~ as.numeric(pre_13_mort[4,3]),
      TRUE ~ Mort_pre_13
    ),
    Mort_rateBik = case_when(
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & Assemblage == "Yelloweye" & Release_Method == "DRM" ~ as.numeric(ye_bik[1,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Charter" & Assemblage == "Yelloweye" & Release_Method == "Surface" ~ as.numeric(ye_bik[2,3]),

      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & Assemblage == "Yelloweye" & Release_Method == "DRM" ~ as.numeric(ye_bik[3,3]),
      CFMU %in% c("CI", "NORTHEAST") & USER == "Private" & Assemblage == "Yelloweye" & Release_Method == "Surface" ~ as.numeric(ye_bik[4,3]),
      TRUE ~ Mort_rateBik
    )
  ) 
########


#########
## Apportioned mortality rates
# This is the post 2013 apportioned mortality rate for pelagic or yelloweye rockfish by user and management area. Takes into account depth of capture and method of release.
App_m_r <- table3.2 %>% 
  mutate(App_mort_rate = P_rel_surf_v_DRM * Mort_rateBik) %>% 
  group_by(CFMU, USER, Assemblage) %>% 
  summarise(
    App_mort_rate = sum(App_mort_rate)
  )


table3.3 <- table3.2 %>% 
  inner_join(App_m_r, by = c("CFMU", "USER", "Assemblage")) 

## Adding Afognak, WKMA, SKMA, and Eastside values using Northeast as a proxy
  # These CFMUs do not have available data, nearest CFMU is used instead
northeast_rows <- table3.3 %>%
  filter(CFMU == "NORTHEAST")

# Create new rows for AFOGNAK, WKMA, SKMA, and EASTSIDE and combine them with the original dataset
new_cfmu_values <- c("AFOGNAK", "WKMA", "SKMA", "EASTSIDE")

table3 <- table3.3 %>%
  bind_rows(map_df(new_cfmu_values, ~mutate(northeast_rows, CFMU = .)))


############################# Table 4 ##########################################

## Black and Yelloweye rockfish release mortality estimates in numbers of fish 
## and variance by Comercial Fisheries Management Unit (CFMU), user, and year.


# Release numbers * table3
table4 <- table1 %>% 
  inner_join(table3, by = c("CFMU", "USER", "Assemblage"), relationship = "many-to-many") %>% 
  group_by(CFMU, Year, USER, Assemblage) %>% 
  reframe(
    Release_Rdoti = case_when(
      Year < 2013 ~ Release * Mort_pre_13,
      Year >= 2013 ~ Release * (App_mort_rate)
    ),

      ## Variance in Release numbers times mortality
    VarRelease_Rdoti = case_when(
      Year < 2013 ~ VarRelease * Mort_pre_13,
      Year >= 2013 ~ VarRelease * (App_mort_rate)
    )
  ) %>% distinct()

########################### Table 5 ############################################

## Sample Sizes by Species
## Provide alternate mean weight (predwt2) using regressions based on larger sample sizes and ln-ln
## transformed l and w relationsships for data through ln-ln transformed l and w, relationships for data through 2021
## (Source for parameter estimates: O:/DSF/GOAB/R Code/RFLW.R)


rf_predwt <- rf %>% 
  mutate(
    # Fork length collected starting in 2020, converting these fork lengths to total lengths to be uniform across all years
    LENGTH = case_when(
      YEAR >= 2020 ~ (LENGTH + 1.421) / (0.983), 
      TRUE ~ LENGTH
    ),
    predwt2 = case_when(
      SP == 138 ~ exp(0.2545639^2/2) * exp(-11.0070042) * LENGTH^3.032305,
      SP == 142 ~ exp(0.1424035^2/2) * exp(-9.4461439) * LENGTH^2.612433,
      SP == 145 ~ exp(0.1514268^2/2) * exp(-10.7841555) * LENGTH^2.974902,
      SP == 146 ~ exp(0.1997301^2/2) * exp(-9.8791339) * LENGTH^2.694802,
      SP == 147 ~ exp(0.1786091^2/2) * exp(-9.5542345) * LENGTH^2.658838,
      SP == 148 ~ exp(0.1730860^2/2) * exp(-11.1333017) * LENGTH^3.088443,
      SP == 149 ~ exp(0.222809^2/2) * exp(-9.5182168) * LENGTH^2.657662,
      SP == 151 ~ exp(0.1215698^2/2) *  exp(-10.5328692) * LENGTH^2.895547,
      SP == 152 ~ 0.00000985 * LENGTH^3.13, # Why is shortraker different?
      SP == 154 ~ exp(0.1960328^2/2) * exp(-10.2540431) * LENGTH^2.809344,
      SP == 155 ~ exp(0.1318010^2/2) * exp(-10.7756447) * LENGTH^2.929560,
      SP == 157 ~ exp(0.2306008^2/2) * exp(-10.0750312) * LENGTH^2.704643,
      SP == 169 ~ exp(0.1644541^2/2) * exp(-10.9876172) * LENGTH^3.020358,
      SP == 172 ~ exp(0.1797718^2/2) * exp(-9.2640429) * LENGTH^2.548944,
      SP == 173 ~ exp(0.1966187^2/2) * exp(-8.9656815) * LENGTH^2.482820,
      TRUE ~ case_when( # Not sure where these calculations come from
        ASSEMB == "Pelagic" ~ (10^-4.51687) * LENGTH^2.84619,
        ASSEMB == "Demersal" ~ (10^-4.74572) * LENGTH^3.00420,
        ASSEMB == "Slope" ~ (10^-4.58089) * LENGTH^2.80292,
        TRUE ~ NA_real_
      )
    ),
    USER = case_when(
      USER == "SewMilC" ~ "Charter",
      TRUE ~ USER
    ) 
  ) %>% 
  filter(SP %in% c(145, 142),
         USER != '')
## Mean Weight by species
MeanWt <- rf_predwt %>%
  group_by(CFMU, YEAR, USER, SP) %>%
  summarize(
    nomeas = n(),
    meankg2 = mean(predwt2, na.rm = TRUE),
    SEkg2 = sd(predwt2, na.rm = TRUE) / sqrt(nomeas),
    meanlb2 = meankg2 * 2.20462,
    SElb2 = SEkg2 * 2.20462
  ) %>%
  ungroup()

## Black and Yelloweye rockfish mean weight for CI and NORTHEAST and PWSO private, all years combined

MeanWt_yrcombi <- rf_predwt %>%
  group_by(CFMU, USER, SP) %>%
  summarize(
    nomeas = n(),
    meankg2 = mean(predwt2, na.rm = TRUE),
    SEkg2 = sd(predwt2, na.rm = TRUE) / sqrt(nomeas),
    meanlb2 = meankg2 * 2.20462,
    SElb2 = SEkg2 * 2.20462
  ) %>%
  ungroup()



table5.1 <- MeanWt %>%
  left_join(MeanWt_yrcombi, by = c("CFMU", "USER", "SP")) %>%
  mutate(meanlb2 = ifelse(nomeas.x < 50, meanlb2.y, meanlb2.x),
         Assemblage = case_when(
           SP == 142 ~ "Black",
           SP == 145 ~ "Yelloweye"
         )) %>%
  rename(Year = YEAR) %>% 
  select(CFMU, Year, USER, Assemblage, meanlb2) 

## Adding Afognak, WKMA, SKMA, and Eastside values using Northeast as a proxy
northeast_rows_5 <- table5.1 %>%
  filter(CFMU == "NORTHEAST")

# Create new rows for AFOGNAK, WKMA, SKMA, and EASTSIDE and combine them with the original dataset
new_cfmu_values <- c("AFOGNAK", "WKMA", "SKMA", "EASTSIDE")

table5 <- table5.1 %>%
  bind_rows(map_df(new_cfmu_values, ~mutate(northeast_rows_5, CFMU = .)))


#################### Table 6 ###################################################
# Release Mortality Weights
table6 <- table4 %>% 
  inner_join(table5, by = c("CFMU", "Year", "USER", "Assemblage")) %>%
  mutate(
    Release_Mort_lbs = Release_Rdoti * meanlb2,
    VarRelease_Mort_lbs = VarRelease_Rdoti * meanlb2
  ) %>% 
  select(-Release_Rdoti, -VarRelease_Rdoti, -meanlb2)

#################### Table 7 ###################################################
# Harvest Weights
table7 <- table1 %>% 
  inner_join(table5, by = c("CFMU", "Year", "USER", "Assemblage")) %>% 
  mutate(
    Harvest_lbs = Harvest * meanlb2,
    VarHarvest_lbs = VarHarvest * meanlb2
  ) %>% 
  select(-Release, -VarRelease, -Harvest, -VarHarvest, -meanlb2)

#################### Table 8 ###################################################
# Total Weight (lbs) of removals

table8 <- table6 %>%
  inner_join(table7, by = c("CFMU", "Year", "USER", "Assemblage")) %>%
  mutate(
    Total_Removals_lbs = Release_Mort_lbs + Harvest_lbs,
    VarTotal_Removals_lbs = VarRelease_Mort_lbs + VarHarvest_lbs,
    Upper_Lower_95 = sqrt(VarTotal_Removals_lbs) * 1.96
  ) %>%
  select(-Release_Mort_lbs, -VarRelease_Mort_lbs, -Harvest_lbs, -VarHarvest_lbs)


################################################################################
####################### Write out data #########################################
################################################################################
library(xlsx)

## Table 1
write.xlsx(table1, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 1 - Release and Harvest", col.names = TRUE, row.names = FALSE,
           append = FALSE)

## Table 2
table2_out <- as.data.frame(table2)
write.xlsx(table2_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 2 - Release by Depth Cat.", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 3
table3_out <- as.data.frame(table3)
write.xlsx(table3_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 3 - Release Mortality", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 4
table4_out <- as.data.frame(table4)
write.xlsx(table4_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 4 - Rel. Mort. # of Fish", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 5
table5_out <- as.data.frame(table5)
write.xlsx(table5_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 5 - Mean Weights of Fish", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 6
table6_out <- as.data.frame(table6)
write.xlsx(table6_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 6 - Release Mortality Wt", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 7
write.xlsx(table7, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 7 - Harvest Wt", col.names = TRUE, row.names = FALSE,
           append = TRUE)

## Table 8
table8_out <- as.data.frame(table8)
write.xlsx(table8_out, file = "reports/RF_Harvest_Reconstruction/RF_Removals_thru23.xlsx",
           sheetName = "Table 8 - Total Removals Wt", col.names = TRUE, row.names = FALSE,
           append = TRUE)


################################################################################
####################### Summary Figures ########################################
################################################################################

## Filter data by species
dat8 <- table6 %>%
  inner_join(table7, by = c("CFMU", "Year", "USER", "Assemblage")) %>%
  mutate(
    Total_Removals_lbs = Release_Mort_lbs + Harvest_lbs,
    VarTotal_Removals_lbs = VarRelease_Mort_lbs + VarHarvest_lbs,
    Upper_Lower_95 = sqrt(VarTotal_Removals_lbs) * 1.96
  )

brf8 <- dat8 %>% 
  filter(Assemblage == "Black", CFMU %in% c("CI", "NG", "NORTHEAST", "PWSI", "PWSO"))

ye8 <- dat8 %>% 
  filter(Assemblage == "Yelloweye", CFMU %in% c("CI", "NG", "NORTHEAST", "PWSI", "PWSO"))
## Gather data for side-by-side bar chart
brf8_gather <- brf8 %>% 
  gather(type, lbs, Release_Mort_lbs:Harvest_lbs)

ye8_gather <- ye8 %>% 
  gather(type, lbs, Release_Mort_lbs:Harvest_lbs)

# Release and Harvest by CFMU
ggplot(data = brf8_gather, aes(x = Year, y = lbs, fill = type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~CFMU)+
  scale_fill_manual(values = c("Release_Mort_lbs" = "black", "Harvest_lbs" = "grey"), labels = c("Release", "Harvest")) +
  labs(title = "Black Rockfish Release and Harvest by Year (lbs)", y = "") +
  theme_minimal()

ggplot(data = ye8_gather, aes(x = Year, y = lbs, fill = type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_wrap(~CFMU)+
  scale_fill_manual(values = c("Release_Mort_lbs" = "black", "Harvest_lbs" = "grey"), labels = c("Release", "Harvest")) +
  labs(title = "Yelloweye Rockfish Release and Harvest by Year (lbs)", y = "") +
  theme_minimal()

# Total removals
ggplot(data = brf8, aes(x = Year, y = Total_Removals_lbs, color = CFMU, fill = CFMU)) +
  geom_smooth() + 
  labs(title = "Total Removals in pounds by CFMU for black rockfish, 1998 - 2023", y = "") + 
  theme_minimal()

ggplot(data = ye8, aes(x = Year, y = Total_Removals_lbs, color = CFMU, fill = CFMU)) +
  geom_smooth() +
  labs(title = "Total Removals in pounds by CFMU for yelloweye rockfish, 1998 - 2023", y = "") + 
  theme_minimal()

