################################################################################
# Objective 3 -
# Estimate the species composition by port of the rockfish harvest landed at Kodiak, 
# Homer, Seward, Whittier, and Valdez during May through September such that the 
# estimated proportions of each species are within 0.20 of the true proportions at 
# least 95% of the time.
#
# Translated from SAS to R by CWM on 02/04/25
################################################################################

library(tidyverse)
library(gt)
source("functions.R")


get_data("data/RF/")


###

library(plyr)
rock <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, rock2004,
                               rock2005, rock2006, rock2007, rock2008, rock2009, rock2010,
                               rock2011, rock2012, rock2013, rock2014, rock2015, rock2016,
                               rock2017, rock2018, rock2019, rock2020, rock2021, rock2022,
                               rock2023, rock2024))
detach(package:plyr)



rf <- rock %>% 
  filter(
    ASSEMB != '',
    !(PORT %in% c('CCI', 'Cordova')), # Not enough data to meaningfully use
  ) %>% 
  mutate(
    USER = case_when(
      USER == '' ~ 'Unknown',
      TRUE ~ USER
    ),
    # Pool VAldez and Whittier data for 1991 (species comp and mean weight weighted
    # by sample size) because harvest not broken out by EPWS or WPWS until 1999.
    # Let Valdez represent PWS 1992 through 1998.
    # Estimate biomass seperately for EPWS (Valdez) and WPWS (Whittier) starting in 1999.
    PORT = case_when(
      PORT == 'Whittier' & YEAR == 1991 ~ 'PWS',
      PORT == 'Valdez' & YEAR >= 1991 & YEAR <= 1998 ~ 'PWS',
      TRUE ~ PORT
    ),
    SPECIES = case_when(
      SP == 154 ~ 'DuskyDrk',
      TRUE ~ SPECIES
    )
  )

## Annual rockfish saples by user group
table(rf$USER, rf$PORT, rf$YEAR, useNA = "always")
## Annual rockfish samples by species
table(rf$SPECIES, rf$PORT, rf$YEAR, useNA = "always")

######
# Get Species Composition from AWL Data
#####
SPComp1 <- rf %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    Total = n()
  )

SPComp2 <- rf %>% 
  group_by(PORT, USER, YEAR, SP) %>% 
  reframe(
    Frequency = n()
  )

SPComp <- merge(SPComp1, SPComp2, by = c('PORT', 'YEAR', 'USER')) %>% 
  group_by(PORT, USER, YEAR, SP) %>% 
  reframe(
    Frequency = Frequency,
    Pct_Frequency = Frequency / Total * 100
  )

## Get total sample size for species composition
SPComp_tot <- merge(SPComp, SPComp1, by = c('PORT', 'USER', 'YEAR')) %>% 
  group_by(PORT, USER, YEAR, SP) %>% 
  reframe(
    Total = Total,
    nj = Frequency,
    pj = Pct_Frequency / 100,
    vpj = pj * (1 - pj) / (Total - 1),
    SEpj = sqrt(vpj)
  )


gt(
  SPComp_tot
) %>% 
  tab_header(
    title = 'Objective 3: Rockfish species composition by port and user'
    )

