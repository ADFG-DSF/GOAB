################################################################################
# Secondary Objective 4 - 
# To refine discard mortality estimates, gather data on the depths of capture for 
# pelagic and nonpelagic rockfish that were released.
#
# Created by CWM on 02/05/25
################################################################################

library(tidyverse)
library(gt)
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


rf_rel <- int %>%
  filter(YEAR >= 2013, # No information on method of release or depth of capture prior to 2013
         TRFREL != 0) %>%  # Don't care about trips where no RF released
  mutate(
    # Release Categories 1-4. 
    # Coding release depth catergories to match those used in harvest reconstruction
    PelRelCat = case_when(
      PELDEPTH >= 1 & PELDEPTH <= 59 ~ 1, # Category 1 depth between 1 and 59 ft
      PELDEPTH >= 60 & PELDEPTH <= 119 ~ 2, # Category 2 depth between 60 and 119 ft
      PELDEPTH >= 120 & PELDEPTH <= 179 ~ 3, # Category 3 depth between 120 and 179 ft
      PELDEPTH >= 180 ~ 4, # Catergory 4 depth 180+ ft
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
    ))

# Pelagic rockfish releases by release category
pelrel <- rf_rel %>% 
  filter(!is.na(PelRelCat)) %>% 
  group_by(YEAR, PORT, USER, PelRelCat) %>% 
  reframe(
    N_trips = n(),
    pelrsurf = sum(PELRSURF),
    pelrdrm = sum(PELRDRM),
  )

# Yelloweye rockfish releases by release category
yerel <- rf_rel %>% 
  filter(!is.na(YERelCat)) %>% 
  group_by(YEAR, PORT, USER, YERelCat) %>% 
  reframe(
    N_trips = n(),
    yersurf = sum(YERSURF),
    yerdrm = sum(YERDRM),
  )

# Non-Pelagic rockfish releases by release category
nprel <- rf_rel %>% 
  filter(!is.na(NPRelCat)) %>% 
  group_by(YEAR, PORT, USER, NPRelCat) %>% 
  reframe(
    N_trips = n(),
    nprsurf = sum(NPRSURF),
    nprdrm = sum(NPRDRM),
  )


# Look at rate of deepwater release vs surface release
rf_surf_drm <- int %>% 
  filter(
    PORT %in% c('Homer', 'Kodiak', 'Seward', 'Valdez', 'Whittier'), # Select only ports with enough data to be useful
    YEAR >= 2013, # No information on method of release or depth of capture prior to 2013
    TRFREL > 0) %>%  # Don't care about trips where no RF released
  group_by(YEAR, PORT, USER) %>% 
  reframe(
    PEL_SURF = sum(PELRSURF, na.rm = TRUE),
    PEL_DRM = sum(PELRDRM, na.rm = TRUE),
    TOT_PEL = PEL_SURF + PEL_DRM,
    Pct_PEL_DRM = PEL_DRM / TOT_PEL * 100,
    YE_SURF = sum(YERSURF, na.rm = TRUE),
    YE_DRM = sum(YERDRM, na.rm = TRUE),
    TOT_YE = YE_SURF + YE_DRM,
    Pct_YE_DRM = YE_DRM / TOT_YE * 100,
    NP_SURF = sum(NPRSURF, na.rm = TRUE),
    NP_DRM = sum(NPRDRM, na.rm = TRUE),
    TOT_NP = NP_SURF + NP_DRM,
    Pct_NP_DRM = NP_DRM / TOT_NP * 100,
  )


ggplot(data = rf_surf_drm, aes(x = YEAR)) +
  geom_line(aes(y = Pct_PEL_DRM, color = 'Pelagic')) +
  geom_line(aes(y = Pct_YE_DRM, color = 'Yelloweye')) +
  geom_line(aes(y = Pct_NP_DRM, color = 'Non-Pelagic')) +
  labs(
    x = 'Year',
    y = 'Percent DRM release'
  ) +
  scale_x_continuous(
    limits = c(min(rf_surf_drm$YEAR), max(rf_surf_drm$YEAR)),
                     breaks = seq(min(rf_surf_drm$YEAR), max(rf_surf_drm$YEAR), by = 2)
    )+
  facet_grid(USER ~ PORT) +
  theme_minimal() +
  scale_color_manual(
    name = 'RF Assemblage',
    breaks = c('Pelagic', 'Yelloweye', 'Non-Pelagic'),
    values = c('Pelagic' = 'black', 'Yelloweye' = 'yellow3', 'Non-Pelagic' = 'red')
  )
