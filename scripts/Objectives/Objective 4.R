################################################################################
# Objective 4 -
# Estimate the age, length, and sex composition by port of the principal rockfishes 
# landed at Kodiak, Homer, Seward, Whittier, and Valdez during May through September 
# such that the estimated proportions are within 0.20 of the true proportions at least 
# 95% of the time.
#
# Created using code from Objective 5 (Lingcod) by CWM 02/04/25
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

### Identify Principal rockfish species

principal_sp <- rf %>% 
  group_by(PORT, YEAR, SP) %>% 
  reframe(
    Total = n()
  ) %>% 
  filter(
    Total > 100
  )

unique(principal_sp$SP)
# Only black and yelloweye rockfish show consistently high sample sizes accross ports and years
    # For now, just looking at these two species, others can be added as necessary
    # These are also the only species we have consistent age data for
brf <- rf %>% 
  filter(
    SP == 142
  )

ye <- rf %>% 
  filter(
    SP == 145
  )


###############################
# Black Rockfish
###############################
table(brf$USER, brf$PORT, brf$YEAR)


# Length data
length_brf <- brf %>% 
  filter(
    !is.na(LENGTH)
  )


mean_length_brf <- length_brf %>% 
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
age_brf <- brf %>% 
  filter(
    !is.na(AGE)
  )


mean_age_brf <- age_brf %>% 
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
sex_brf <- brf %>% 
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
  mean_length_brf %>% 
    select(YEAR, PORT, USER, MEANCM, SECM, VARCM, RPCM)
) %>% 
  tab_header(
    title = 'Black Rockfish Length',
    subtitle = 'RPCM is approx. relative precision on mean net wt (95% conf.)'
  )

## Age
gt(
  mean_age_brf %>% 
    select(YEAR, PORT, USER, MEANAGE, SEAGE, VARAGE, RPAGE)
) %>% 
  tab_header(
    title = 'Black Rockfish Age',
    subtitle = 'RPAGE is approx. relative precision on mean net wt (95% conf.)'
  )


## Sex
gt(
  sex_brf
) %>% 
  tab_header(
    title = 'Black Rockfish Sex Ratio (Percent Female)'
  )

###############################
# Yelloweye Rockfish
###############################
table(ye$USER, ye$PORT, ye$YEAR)


# Length data
length_ye <- ye %>% 
  filter(
    !is.na(LENGTH)
  )


mean_length_ye <- length_ye %>% 
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
age_ye <- ye %>% 
  filter(
    !is.na(AGE)
  )


mean_age_ye <- age_ye %>% 
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
sex_ye <- ye %>% 
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
  mean_length_ye %>% 
    select(YEAR, PORT, USER, MEANCM, SECM, VARCM, RPCM)
) %>% 
  tab_header(
    title = 'Yelloweye Rockfish Length',
    subtitle = 'RPCM is approx. relative precision on mean net wt (95% conf.)'
  )

## Age
gt(
  mean_age_ye %>% 
    select(YEAR, PORT, USER, MEANAGE, SEAGE, VARAGE, RPAGE)
) %>% 
  tab_header(
    title = 'Yelloweye Rockfish Age',
    subtitle = 'RPAGE is approx. relative precision on mean net wt (95% conf.)'
  )


## Sex
gt(
  sex_ye
) %>% 
  tab_header(
    title = 'Yelloweye Rockfish Sex Ratio (Percent Female)'
  )
