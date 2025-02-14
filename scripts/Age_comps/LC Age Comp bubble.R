#############################################################################
# Create bubble plots of LC ages
# By Mgmt area
#
# Created by CWM on 06/13/24
# Updated to look at ages in each management area plus all SC 02/13/25
#############################################################################

library(tidyverse)


source("functions.R")

#Function call
get_data("data/LC/")

#####
library(plyr)

lc <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                                   ling2009, ling2010, ling2011, ling2012, ling2013, 
                                   ling2014, ling2015, ling2016, ling2017, ling2018, 
                                   ling2019, ling2020, ling2021, ling2022, ling2023))
detach(package:plyr)

lc$AGE <- as.integer(lc$AGE)

lc_all <- lc %>% mutate(
  USER = case_when(
    USER == 'P' ~ 'Private',
    USER == 'C' | USER == 'SewMilC' ~ 'Charter',
    TRUE ~ USER
  )
)



###############################
# Split into Mgmt Areas
###############################
lc_split <- area_split_sf(lc_all) %>% 
  select(-X) 

######### Age ################

# Filter rows with non-missing age
agecomp <- lc_split %>%
  filter(
    YEAR >= 1996, 
    !is.na(AGE))%>%
  mutate(
    PORT = (case_when(
      PORT %in% c('Homer', 'CCI') ~ 'CI',
      TRUE ~ PORT
    )))



#AGE
comp_age <- agecomp %>%
  group_by(SFmgmtarea, USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>% 
  ##restructure data file so sample size (nj) by each user group is a separate variable,
  ##all on one line for each species
  # Assign values to respective variables based on user value
  mutate(nijC = if_else(USER == 'Charter', COUNT, 0),
         nijP = if_else(USER == 'Private', COUNT, 0),
         nijU = if_else(USER == 'Unknown', COUNT, 0),
         nijM = if_else(USER == 'SewMilC', COUNT, 0)) %>%
  select(-COUNT)



# Calculate sums of nijC, nijP, nijU, nijM
comp2_age <- comp_age %>%
  group_by(SFmgmtarea, YEAR, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

totaln_age <- comp2_age %>%
  group_by(SFmgmtarea, YEAR) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

comp3_age <- merge(comp2_age, totaln_age, by = c("SFmgmtarea", "YEAR")) %>% 
  mutate(
    pij = (nijC + nijP + nijU + nijM) / sum(niC + niP + niU + niM),
    n = (niC + niP + niU + niM)
  )
##Cohorts
plotages <- comp3_age %>%
  mutate(Cohort = YEAR - AGE) 

##############################
# Plots
##############################


#### PWS
lc_PWS <- plotages %>% 
  filter(
    SFmgmtarea == 'PWS'
    )

scale_PWS <- max(lc_PWS$n) / max(lc_PWS$AGE)


lc_age_PWS <- ggplot(data = lc_PWS) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_PWS), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(lc_PWS$YEAR), 3), labels = as.character(seq(1995, max(lc_PWS$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(lc_PWS$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_PWS, name = 'Sample Size', breaks = seq(0, 700, 100))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'SFmgmtarea=PWS') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_PWS




#### Prince William Sound
lc_PWS <- plotages %>% 
  filter(
    SFmgmtarea == 'PWS'
  )

scale_PWS <- max(lc_PWS$n) / max(lc_PWS$AGE)


lc_age_PWS <- ggplot(data = lc_PWS) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_PWS), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(lc_PWS$YEAR), 3), labels = as.character(seq(1995, max(lc_PWS$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(lc_PWS$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_PWS, name = 'Sample Size', breaks = seq(0, 750, 100))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'SFmgmtarea=PWS') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_PWS

#### Cook Inlet
lc_CI <- plotages %>% 
  filter(
    SFmgmtarea == 'CI'
  )

scale_CI <- max(lc_CI$n) / max(lc_CI$AGE)


lc_age_CI <- ggplot(data = lc_CI) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_CI), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(lc_CI$YEAR), 3), labels = as.character(seq(1995, max(lc_CI$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(lc_CI$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_CI, name = 'Sample Size', breaks = seq(0, 250, 50))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'SFmgmtarea=CI') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_CI

#### Kodiak
lc_KOD <- plotages %>% 
  filter(
    SFmgmtarea == 'KOD'
  )

scale_KOD <- max(lc_KOD$n) / max(lc_KOD$AGE)


lc_age_KOD <- ggplot(data = lc_KOD) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_KOD), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(lc_KOD$YEAR), 3), labels = as.character(seq(1995, max(lc_KOD$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(lc_KOD$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_KOD, name = 'Sample Size', breaks = seq(0, 250, 50))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'SFmgmtarea=KOD') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_KOD



#### North Gulf Coast
lc_NG <- plotages %>% 
  filter(
    SFmgmtarea == 'NG'
  )

scale_NG <- max(lc_NG$n) / max(lc_NG$AGE)


lc_age_NG <- ggplot(data = lc_NG) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_NG), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(lc_NG$YEAR), 3), labels = as.character(seq(1995, max(lc_NG$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(lc_NG$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_NG, name = 'Sample Size', breaks = seq(0, 200, 50))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'SFmgmtarea=NG') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_NG

#### All Southcentral
# Due to lower sample sized for lingcod, this will be the more useful data too look at
# Will also look at all southcentral without Kodiak due to no size restrictions in Kodiak, resulting in younger fish than other areas

comp_age_all <- agecomp %>%
  group_by(USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>% 
  ##restructure data file so sample size (nj) by each user group is a separate variable,
  ##all on one line for each species
  # Assign values to respective variables based on user value
  mutate(nijC = if_else(USER == 'Charter', COUNT, 0),
         nijP = if_else(USER == 'Private', COUNT, 0),
         nijU = if_else(USER == 'Unknown', COUNT, 0),
         nijM = if_else(USER == 'SewMilC', COUNT, 0)) %>%
  select(-COUNT)



# Calculate sums of nijC, nijP, nijU, nijM
comp2_age_all <- comp_age_all %>%
  group_by(YEAR, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

totaln_age_all <- comp2_age_all %>%
  group_by(YEAR) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

comp3_age_all <- merge(comp2_age_all, totaln_age_all, by = c("YEAR")) %>% 
  mutate(
    pij = (nijC + nijP + nijU + nijM) / sum(niC + niP + niU + niM),
    n = (niC + niP + niU + niM)
  )
##Cohorts
plotages_all <- comp3_age_all %>%
  mutate(Cohort = YEAR - AGE) 

## Plot
scale_all <- max(plotages_all$n) / max(plotages_all$AGE)


lc_age_all <- ggplot(data = plotages_all) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_all), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(plotages_all$YEAR), 3), labels = as.character(seq(1995, max(plotages_all$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(plotages_all$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_all, name = 'Sample Size', breaks = seq(0, 1000, 200))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'Southcentral') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_all

#### Southcentral minus Kodiak
## All other areas share 35 in. minimum size restriction, should see less younger fish
comp_age_nok <- agecomp %>%
  filter(SFmgmtarea != 'KOD') %>% 
  group_by(USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  ungroup() %>% 
  ##restructure data file so sample size (nj) by each user group is a separate variable,
  ##all on one line for each species
  # Assign values to respective variables based on user value
  mutate(nijC = if_else(USER == 'Charter', COUNT, 0),
         nijP = if_else(USER == 'Private', COUNT, 0),
         nijU = if_else(USER == 'Unknown', COUNT, 0),
         nijM = if_else(USER == 'SewMilC', COUNT, 0)) %>%
  select(-COUNT)



# Calculate sums of nijC, nijP, nijU, nijM
comp2_age_nok <- comp_age_nok %>%
  group_by(YEAR, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

totaln_age_nok <- comp2_age_nok %>%
  group_by(YEAR) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

comp3_age_nok <- merge(comp2_age_nok, totaln_age_nok, by = c("YEAR")) %>% 
  mutate(
    pij = (nijC + nijP + nijU + nijM) / sum(niC + niP + niU + niM),
    n = (niC + niP + niU + niM)
  )
##Cohorts
plotages_nok <- comp3_age_nok %>%
  mutate(Cohort = YEAR - AGE) 

## Plot
scale_nok <- max(plotages_nok$n) / max(plotages_nok$AGE)


lc_age_nok <- ggplot(data = plotages_nok) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_nok), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, max(plotages_nok$YEAR), 3), labels = as.character(seq(1995, max(plotages_nok$YEAR), 3)), name = 'Year',
                     limit = c(1995, max(plotages_nok$YEAR))) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_nok, name = 'Sample Size', breaks = seq(0, 1000, 200))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Lingcod Age Comps',
       subtitle = 'Southcentral minus KOD') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

lc_age_nok

