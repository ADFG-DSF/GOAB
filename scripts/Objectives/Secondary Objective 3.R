################################################################################
# Secondary Objective 3 -
# Estimate the proportions of released Pacific halibut that were caught on circle 
# hooks versus other types of hooks at each port. This information is needed to 
# refine estimates of halibut release mortality in the sport fishery.
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


pCirc <- int %>% 
  filter(
    YEAR >= 2007, # Hook type becan being recorded in 2007
    PORT %in% c('Homer', 'Kodiak', 'Seward', 'Whittier', 'Valdez'), # Only select ports with enough data to be meaningful
    USER %in% c('Private', 'Charter') # Ignore entries with no recorded user and Military due to lack of data
  ) %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    SUMCirc = sum(HARELCIR, na.rm = TRUE),
    SUMOth = sum(HARELOTH, na.rm = TRUE),
    AllRel = SUMOth + SUMCirc,
    PCirc = SUMCirc / AllRel * 100
  )

gt(
  pCirc
) %>% 
  tab_header(title = 'Percent of halibut released on circle hooks vs other hook types')


ggplot(data = pCirc, aes(x = YEAR, y = PCirc)) +
  geom_line() +
  labs(
    x = 'Year',
    y = 'Percent Released',
    title = 'Percent of halibut released on circle hooks vs other hook types'
  ) +
  facet_grid(USER ~ PORT)
