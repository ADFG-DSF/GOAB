################################################################################
# Secondary Objective 2 -
# Estimate the proportion of the Pacific halibut harvest that was cleaned (and 
# carcasses discarded) at sea at each port. These estimates will be used to stratify 
# length and weight estimates at ports where cleaning at sea is prevalent.
#
# This code comes from Objectives 1 & 2 where percent cleaned at sea is used for Homer
# charter halibut estimates
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
                                int18, int19, int20, int21, int22, int23))
detach(package:plyr)



CAS <- int %>% 
  filter(
    PORT %in% c('Homer', 'Kodiak', 'Seward', 'Whittier', 'Valdez'), # Only select ports with enough data to be meaningful
    USER %in% c('Private', 'Charter') # Ignore entries with no recorded user and Military due to lack of data
  ) %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    SUMCAS = sum(HACAS, na.rm = TRUE),
    SUMKEPT = sum(HAKEPT, na.rm = TRUE),
    PCAS = SUMCAS / SUMKEPT * 100
  )

gt(
  CAS
) %>% 
  tab_header(title = 'Percent of halibut cleaned at sea by port and user group')


ggplot(data = CAS, aes(x = YEAR, y = PCAS)) +
  geom_line() +
  labs(
    x = 'Year',
    y = 'Percent Halibut cleaned at sea'
  ) +
  facet_grid(USER ~ PORT)
# Mean proportion of halibut cleaned at sea for vessel trips with halibut cleaned at sea only
fracCAS <- int %>% 
  filter(
    PORT %in% c('Homer', 'Kodiak', 'Seward', 'Whittier', 'Valdez'), # Only select ports with enough data to be meaningful
    USER %in% c('Private', 'Charter'), # Ignore entries with no recorded user and Military due to lack of data
    HACAS > 0 # Select only trips that cleaned halibut at sea
  ) %>% 
  mutate(
    PCAS = HACAS / HAKEPT
  ) %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    Mean_pCAS = mean(PCAS)
  )


gt(fracCAS) %>% 
  tab_header(title = 'For trips with halibut cleaned at sea, the mean proportion that were cleaned at sea')

ggplot(data = fracCAS, aes(x = YEAR, y = Mean_pCAS)) +
  geom_line() +
  labs(
    x = 'Year',
    y = 'Proportion cleaned at sea',
    title = 'Mean Proportion of halibut cleaned at sea, \nby vessels that cleaned halibut at sea'
  ) +
  facet_grid(USER ~ PORT)
