################################################################################
# Secondary Objective 1 - 
# Identify differences in the geographic distribution of groundfish effort and 
# harvest between user groups and across years for each port during May through September.
#
# Using interview data to look at distribution of harvest by user group and port each year.
#
# Created by CWM on 02/05/25
################################################################################

library(tidyverse)
library(gt)
library(sf)
library(maps)

source("functions.R")


get_data("data/Intervw/")


###

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021

int22$YEAR <- 2022

library(plyr)
int <- do.call(rbind.fill, list(intervw9219, int9204, int05, int06, int07, 
                                int08, int09, int10, int11, int12, int13, 
                                int14, int15, int16, int16prelim, int17, 
                                int18, int19, int20, int21, int22, int23,
                                int24))
detach(package:plyr)

# ADFG Stat Area shapefiles
stat_areas <- read_sf("data/Spatial_Data/",
                      "State_Stat_Area_Simple")
# State of Alaska shapefile
Alaska <- read_sf("data/Spatial_Data/",
                  "Alaska_Simple")

## Halibut 
hal_int <- int %>% 
  mutate(
    STAT_AREA = case_when(
      !is.na(ADFGSTATHAL) ~ ADFGSTATHAL, # If the interview has a halibut specific stat area this is used
      is.na(ADFGSTATHAL) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI, # if the interview has a combined area (multi target) and no halibut, this is used
      is.na(ADFGSTATHAL) & is.na(ADFGSTATCOMBI)  & !is.na(ADFGSTAT) ~ ADFGSTAT
    ),
    HAL_HOURS = case_when(
      HOURS_H > 0 ~ HOURS_H,
      HOURS_H == 0 & HOURS_COMBI!= 0 ~ HOURS_COMBI, #
      HOURS_H == 0 & (HOURS_COMBI == 0 | is.na(HOURS_COMBI)) & HOURS_TOT == 0 ~ HOURS_L + HOURS_R,
      !is.na(WHOLEHRS) ~ WHOLEHRS,
      TRUE ~ HOURS_TOT
    ),
    # Attempting to dig into hours actually targetting halibut
    HOURS_H2 = case_when(
      !is.na(HOURS_H) ~ HOURS_H,
      is.na(HOURS_H) & HAKEPT > 0 & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      TRUE ~ 0
    ),
    TOT_HOURS = (ANGLDAYS * HAL_HOURS)/NUMDAYS
  ) %>% 
  group_by(YEAR, PORT, USER, STAT_AREA) %>% 
  reframe(
    HAL_Harv = sum(HAKEPT, na.rm = TRUE),
    HAL_Eff_Hour = sum(TOT_HOURS, na.rm = TRUE),
    HAL_target_hour = sum(HOURS_H2, na.rm = TRUE)
  ) %>% 
  filter(
    !is.na(STAT_AREA), # Remove entries without stat area data
    YEAR >= 2000 # don't have hours recorded before 2000
    )

hal_areas <- subset(stat_areas, STAT_AREA %in% hal_int$STAT_AREA)

hal_plot <- merge(hal_areas, hal_int, by = "STAT_AREA")

hal_box <- st_bbox(hal_areas)

gt(hal_int) %>% 
  tab_header(title = 'Spatial distribution of halibut harvest and effort data by Port, Year, and User')

# Lingcod
ling_int <- int %>% 
  mutate(
    STAT_AREA = case_when(
      !is.na(ADFGSTATLING) ~ ADFGSTATLING, # If the interview has a halibut specific stat area this is used
      is.na(ADFGSTATLING) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI, # if the interview has a combined area (multi target) and no halibut, this is used
      is.na(ADFGSTATLING) & is.na(ADFGSTATCOMBI)  & !is.na(ADFGSTAT) ~ ADFGSTAT
    ),
    LC_HOURS = case_when(
      HOURS_L > 0 ~ HOURS_L,
      HOURS_L == 0 & HOURS_COMBI!= 0 ~ HOURS_COMBI, #
      HOURS_L == 0 & (HOURS_COMBI == 0 | is.na(HOURS_COMBI)) & HOURS_TOT == 0 ~ HOURS_R + HOURS_H,
      !is.na(WHOLEHRS) ~ WHOLEHRS,
      TRUE ~ HOURS_TOT
    ),
    # Attempting to dig into hours actually targetting lingcod
    HOURS_L2 = case_when(
      !is.na(HOURS_L) ~ HOURS_L,
      is.na(HOURS_L) & LCKEPT > 0 & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      TRUE ~ 0
    ),
    TOT_HOURS = (ANGLDAYS * LC_HOURS)/NUMDAYS
  ) %>% 
  group_by(YEAR, PORT, USER, STAT_AREA) %>% 
  reframe(
    LC_Harv = sum(LCKEPT, na.rm = TRUE),
    LC_Eff_Hour = sum(TOT_HOURS, na.rm = TRUE),
    LC_target_hour = sum(HOURS_L2, na.rm = TRUE)
  ) %>% 
  filter(
    !is.na(STAT_AREA), # Remove entries without stat area data
    YEAR >= 2000 # don't have hours recorded before 2000
  )

ling_areas <- subset(stat_areas, STAT_AREA %in% ling_int$STAT_AREA)

ling_plot <- merge(ling_areas, ling_int, by = "STAT_AREA")

ling_box <- st_bbox(ling_areas)

gt(ling_int) %>% 
  tab_header(title = 'Spatial distribution of lingcod harvest and effort data by Port, Year, and User')

# Rockfish
rf_int <- int %>% 
  mutate(
    STAT_AREA = case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF, # If the interview has a halibut specific stat area this is used
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI, # if the interview has a combined area (multi target) and no halibut, this is used
      is.na(ADFGSTATRF) & is.na(ADFGSTATCOMBI)  & !is.na(ADFGSTAT) ~ ADFGSTAT
    ),
    RF_HOURS = case_when(
      HOURS_R > 0 ~ HOURS_R,
      HOURS_R == 0 & HOURS_COMBI!= 0 ~ HOURS_COMBI, #
      HOURS_R == 0 & (HOURS_COMBI == 0 | is.na(HOURS_COMBI)) & HOURS_TOT == 0 ~ HOURS_L + HOURS_H,
      !is.na(WHOLEHRS) ~ WHOLEHRS,
      TRUE ~ HOURS_TOT
    ),
    # Attempting to dig into hours actually targetting lingcod
    HOURS_R2 = case_when(
      !is.na(HOURS_R) ~ HOURS_R,
      is.na(HOURS_R) & TRFKEPT > 0 & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      TRUE ~ 0
    ),
    TOT_HOURS = (ANGLDAYS * RF_HOURS)/NUMDAYS
  ) %>% 
  group_by(YEAR, PORT, USER, STAT_AREA) %>% 
  reframe(
    RF_Harv = sum(TRFKEPT, na.rm = TRUE),
    RF_Eff_Hour = sum(TOT_HOURS, na.rm = TRUE),
    RF_target_hour = sum(HOURS_R2, na.rm = TRUE)
  ) %>% 
  filter(
    !is.na(STAT_AREA), # Remove entries without stat area data
    YEAR >= 2000 # don't have hours recorded before 2000
  )

rf_areas <- subset(stat_areas, STAT_AREA %in% rf_int$STAT_AREA)

rf_plot <- merge(rf_areas, rf_int, by = "STAT_AREA")

rf_box <- st_bbox(rf_areas)

gt(rf_int) %>% 
  tab_header(title = 'Spatial distribution of rockfish harvest and effort data by Port, Year, and User')

###############################################
# Effort - Number of Hours fished by species, and total fishing days
##############################################
# Halibut Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = HAL_target_hour))+
  scale_fill_distiller(palette = "Spectral", "Hours of Halibut effort")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000)) +
  facet_grid(USER ~ PORT)

# Lingcod Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ling_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = LC_target_hour))+
  scale_fill_distiller(palette = "Spectral", "Hours of Lingcod effort")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000)) +
  facet_grid(USER ~ PORT)

# Rockfish Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = RF_target_hour))+
  scale_fill_distiller(palette = "Spectral", "Hours of Rockfish effort")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000)) +
  facet_grid(USER ~ PORT)





###############################
# Harvest - number of fish kept
###############################
# Halibut Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = HAL_Harv))+
  scale_fill_distiller(palette = "Spectral", "Halibut Harvest")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000)) +
  facet_grid(USER ~ PORT)

# Lingcod Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ling_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = LC_Harv))+
  scale_fill_distiller(palette = "Spectral", "Lingcod Harvest")+
  coord_sf(xlim = c(ling_box[1] - 100000, ling_box[3] + 100000), 
           ylim = c(ling_box[2] - 100000, ling_box[4] + 100000)) +
  facet_grid(USER ~ PORT)

# Rockfish Effort
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot %>%  filter (YEAR == 2024), # Choose year to plot
          color = "black", aes(fill = RF_Harv))+
  scale_fill_distiller(palette = "Spectral", "Rockfish Harvest")+
  coord_sf(xlim = c(rf_box[1] - 100000, rf_box[3] + 100000), 
           ylim = c(rf_box[2] - 100000, rf_box[4] + 100000)) +
  facet_grid(USER ~ PORT)
