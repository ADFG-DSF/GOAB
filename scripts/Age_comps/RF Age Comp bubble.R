#############################################################################
# Create bubble plots of RF ages
# By Mgmt area
#
# Created by CWM on 01/25/24
# Modified to include plots by management area for all Sport Fish management 
# areas - CWM 02/10/25
#############################################################################
library(tidyverse)


source("functions.R")

#Function call
get_data("data/RF/")

#####
library(plyr)

rf_all <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                   rock2004, rock2005, rock2006, rock2007, rock2008, 
                                   rock2009, rock2010, rock2011, rock2012, rock2013, 
                                   rock2014, rock2015, rock2016, rock2017, rock2018, 
                                   rock2019, rock2020, rock2021, rock2022, rock2023))
detach(package:plyr)

##########
# Separate into
########
rf_split <- area_split_sf(rf_all) 


# Filter rows with non-missing age
agecomp <- rf_split %>%
  filter(!is.na(AGE))%>%
  filter(!(SP %in% c(144, 168, 169))) %>%
  mutate(SPECIES = case_when(
    SPECIES == 'Blackgll' ~ 'Blackgill',
    SPECIES == 'Quill' ~ 'Quillback',
    SPECIES == 'Redstrpe' ~ 'Redstripe',
    SPECIES == 'Harleq' ~ 'Harlequin',
    SPECIES == 'Rosethrn' ~ 'Rosethorn',
    SPECIES == 'Shortrkr' ~ 'Shortraker',
    SPECIES == 'Shrpchin' ~ 'Sharpchin',
    SPECIES == 'Silvergr' ~ 'Silvergray',
    SPECIES == 'Vermilon' ~ 'Vermilion',
    SPECIES == 'Yelleye' ~ 'Yelloweye',
    SPECIES == 'Yelltail' ~ 'Yellowtail',
    SPECIES == 'DuskyDark' ~ 'DuskyDark',
    TRUE ~ SPECIES
  ),
  PORT = (case_when(
    PORT %in% c('Homer', 'CCI') ~ 'CI',
    TRUE ~ PORT
  ))) %>% filter (YEAR >= 1996, !is.na(AGE))

##########################################33
# Bubble Plot
##############################################
#AGE
comp_age <- agecomp %>%
  group_by(SFmgmtarea, USER, YEAR, SPECIES, AGE) %>%
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
  group_by(SFmgmtarea, YEAR, SPECIES, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

totaln_age <- comp2_age %>%
  group_by(SFmgmtarea, YEAR, SPECIES) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

comp3_age <- merge(comp2_age, totaln_age, by = c("SFmgmtarea", "YEAR", "SPECIES")) %>% 
  mutate(
    pij = (nijC + nijP + nijU + nijM) / sum(niC + niP + niU + niM),
    n = (niC + niP + niU + niM)
  )
## Use year of capture and age to determine cohort
plotages <- comp3_age %>%
  mutate(Cohort = YEAR - AGE)
# %>%
#   mutate(Cohort = ifelse(Cohort %in% c(1979, 1991, 1996, 2002), NA, Cohort))
#Black rockfish
black <- plotages %>%
  filter(SPECIES == 'Black')
#Yelloweye rockfish
Yelloweye <- plotages %>%
  filter(SPECIES == 'Yelloweye')



######################
# Plots
######################


#### Prince William Sound
# Black rockfish
black_PWS <- black %>% 
  filter(
    SFmgmtarea == 'PWS'
  )


scale_b_PWS <- max(black_PWS$n) / max(black_PWS$AGE)

brf_age_PWS <- ggplot(data = black_PWS) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b_PWS), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(black_PWS$YEAR))) +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b_PWS, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'Prince William Sound') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1991, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 5, label = "1991", color = "red") +
  geom_abline(intercept = -2002, slope = 1, color = 'red') +
  annotate("text", x = 2002, y = 1, label = "2002", color = "red") +
  geom_abline(intercept = -2007, slope = 1, color = 'red') +
  annotate("text", x = 2007, y = 1, label = "2007", color = "red") +
  geom_abline(intercept = -2013, slope = 1, color = 'red') +
  annotate("text", x = 2013, y = 1, label = "2013", color = "red") +
  geom_abline(intercept = -2014, slope = 1, color = 'red') +
  annotate("text", x = 2014, y = -1, label = "2014", color = "red") 

brf_age_PWS

Yelloweye_PWS <- Yelloweye %>%
  filter(
    SFmgmtarea == 'PWS'
  )

scale_y_PWS <- max(Yelloweye_PWS$n) / max(Yelloweye_PWS$AGE)

ye_age_PWS <- ggplot(data = Yelloweye_PWS) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y_PWS), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(Yelloweye_PWS$YEAR))) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y_PWS, name = 'Sample Size', breaks = seq(0, 800, 200))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'Prince William Sound') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1968, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 28, label = "1968", color = "red") +
  geom_abline(intercept = -1986, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 10, label = "1986", color = "red") +
  geom_abline(intercept = -1998, slope = 1, color = 'red') +
  annotate("text", x = 1998, y = 1, label = "1998", color = "red") 

ye_age_PWS

#### Cook Inlet

# Black rockfish
black_CI <- black %>% 
  filter(
    SFmgmtarea == 'CI'
  )

scale_b_CI <- max(black_CI$n) / max(black_CI$AGE)

brf_age_CI <- ggplot(data = black_CI) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b_CI), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(black_CI$YEAR))) +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b_CI, name = 'Sample Size', breaks = seq(0, 550, 150))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'Cook Inlet') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1979, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 17, label = "1979", color = "red") +
  geom_abline(intercept = -1991, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 5, label = "1991", color = "red") +
  geom_abline(intercept = -1996, slope = 1, color = 'red') +
  annotate("text", x = 1996, y = 1, label = "1996", color = "red") +
  geom_abline(intercept = -2002, slope = 1, color = 'red') +
  annotate("text", x = 2002, y = 1, label = "2002", color = "red") +
  geom_abline(intercept = -2008, slope = 1, color = 'red') +
  annotate("text", x = 2008, y = 1, label = "2008", color = "red") +
  geom_abline(intercept = -2013, slope = 1, color = 'red') +
  annotate("text", x = 2013, y = 1, label = "2013", color = "red") +
  geom_abline(intercept = -2014, slope = 1, color = 'red') +
  annotate("text", x = 2014, y = -1, label = "2014", color = "red") 

brf_age_CI

Yelloweye_CI <- Yelloweye %>%
  filter(
    SFmgmtarea == 'CI'
  )

scale_y_CI <- max(Yelloweye_CI$n) / max(Yelloweye_CI$AGE)

ye_age_CI <- ggplot(data = Yelloweye_CI) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y_CI), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(Yelloweye_CI$YEAR))) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y_CI, name = 'Sample Size', breaks = seq(0, 120, 40))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'Cook Inlet') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )
# Not adding cohorts, low sample size

ye_age_CI

#### Kodiak
# Black rockfish
black_KOD <- black %>% 
  filter(
    SFmgmtarea == 'KOD'
  )

scale_b_KOD <- max(black_KOD$n) / max(black_KOD$AGE)

brf_age_KOD <- ggplot(data = black_KOD) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b_KOD), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(black_KOD$YEAR))) +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b_KOD, name = 'Sample Size', breaks = seq(0, 600, 150))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'Kodiak') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1979, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 17, label = "1979", color = "red") +
  geom_abline(intercept = -1990, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 3, label = "1990", color = "red") +
  geom_abline(intercept = -1991, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 5, label = "1991", color = "red") +
  geom_abline(intercept = -1996, slope = 1, color = 'red') +
  annotate("text", x = 1996, y = 1, label = "1996", color = "red") +
  geom_abline(intercept = -2002, slope = 1, color = 'red') +
  annotate("text", x = 2002, y = 1, label = "2002", color = "red") +
  geom_abline(intercept = -2008, slope = 1, color = 'red') +
  annotate("text", x = 2008, y = 1, label = "2008", color = "red") +
  geom_abline(intercept = -2013, slope = 1, color = 'red') +
  annotate("text", x = 2013, y = 1, label = "2013", color = "red") +
  geom_abline(intercept = -2014, slope = 1, color = 'red') +
  annotate("text", x = 2014, y = -1, label = "2014", color = "red") 

brf_age_KOD

Yelloweye_KOD <- Yelloweye %>%
  filter(
    SFmgmtarea == 'KOD'
  )

scale_y_KOD <- max(Yelloweye_KOD$n) / max(Yelloweye_KOD$AGE)

ye_age_KOD <- ggplot(data = Yelloweye_KOD) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y_KOD), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(Yelloweye_KOD$YEAR))) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y_KOD, name = 'Sample Size', breaks = seq(0, 50, 15))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'Kodiak') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )
# Not adding cohorts, low sample size

ye_age_KOD


#### North Gulf Coast
# Black rockfish
black_NG <- black %>% 
  filter(
    SFmgmtarea == 'NG'
  )

scale_b_NG <- max(black_NG$n) / max(black_NG$AGE)

brf_age_NG <- ggplot(data = black_NG) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b_NG), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(black_NG$YEAR))) +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b_NG, name = 'Sample Size', breaks = seq(0, 600, 150))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'North Gulf Coast') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1991, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 5, label = "1991", color = "red") +
  geom_abline(intercept = -1997, slope = 1, color = 'red') +
  annotate("text", x = 1997, y = 1, label = "1997", color = "red") +
  geom_abline(intercept = -2002, slope = 1, color = 'red') +
  annotate("text", x = 2002, y = 1, label = "2002", color = "red") +
  geom_abline(intercept = -2007, slope = 1, color = 'red') +
  annotate("text", x = 2007, y = 1, label = "2007", color = "red") +
  geom_abline(intercept = -2013, slope = 1, color = 'red') +
  annotate("text", x = 2013, y = 1, label = "2013", color = "red") +
  geom_abline(intercept = -2014, slope = 1, color = 'red') +
  annotate("text", x = 2014, y = -1, label = "2014", color = "red") 

brf_age_NG

Yelloweye_NG <- Yelloweye %>%
  filter(
    SFmgmtarea == 'NG'
  )

scale_y_NG <- max(Yelloweye_NG$n) / max(Yelloweye_NG$AGE)

ye_age_NG <- ggplot(data = Yelloweye_NG) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y_NG), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(Yelloweye_KOD$YEAR))) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y_NG, name = 'Sample Size', breaks = seq(0, 200, 50))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'North Gulf Coast') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )
# Not adding cohorts, low sample size

ye_age_NG


##### All Southcentral
#AGE
comp_age_all <- agecomp %>%
  group_by(USER, YEAR, SPECIES, AGE) %>%
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
  group_by(YEAR, SPECIES, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

totaln_age_all <- comp2_age_all %>%
  group_by(YEAR, SPECIES) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

comp3_age_all <- merge(comp2_age_all, totaln_age_all, by = c("YEAR", "SPECIES")) %>% 
  mutate(
    pij = (nijC + nijP + nijU + nijM) / sum(niC + niP + niU + niM),
    n = (niC + niP + niU + niM)
  )
##Create tables for appendix
plotages_all <- comp3_age_all %>%
  mutate(Cohort = YEAR - AGE) 
# %>%
#   mutate(Cohort = ifelse(Cohort %in% c(1979, 1991, 1996, 2002), NA, Cohort))
#Black rockfish
black_all <- plotages_all %>%
  filter(SPECIES == 'Black')
#Yelloweye rockfish
Yelloweye_all <- plotages_all %>%
  filter(SPECIES == 'Yelloweye')

## Plots
scale_b_all <- max(black_all$n) / max(black_all$AGE)

brf_age_all <- ggplot(data = black_all) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b_all), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(black_all$YEAR))) +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b_all, name = 'Sample Size', breaks = seq(0, 2650, 650))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'Southcentral') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1991, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 5, label = "1991", color = "red") +
  geom_abline(intercept = -1996, slope = 1, color = 'red') +
  annotate("text", x = 1996, y = 1, label = "1996", color = "red") +
  geom_abline(intercept = -2002, slope = 1, color = 'red') +
  annotate("text", x = 2002, y = 1, label = "2002", color = "red") +
  geom_abline(intercept = -2007, slope = 1, color = 'red') +
  annotate("text", x = 2007, y = 1, label = "2007", color = "red") +
  geom_abline(intercept = -2008, slope = 1, color = 'red') +
  annotate("text", x = 2008, y = -1, label = "2008", color = "red") +
  geom_abline(intercept = -2013, slope = 1, color = 'red') +
  annotate("text", x = 2013, y = 1, label = "2013", color = "red") +
  geom_abline(intercept = -2014, slope = 1, color = 'red') +
  annotate("text", x = 2014, y = -1, label = "2014", color = "red") 

brf_age_all


scale_y_all <- max(Yelloweye_all$n) / max(Yelloweye_all$AGE)

ye_age_all <- ggplot(data = Yelloweye_all) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y_all), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year',
                     limit = c(1995, max(Yelloweye_KOD$YEAR))) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y_all, name = 'Sample Size', breaks = seq(0, 950, 250))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(range = c(2, 15), guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'Southcentral') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  ) +
  # Adding lines to indicate strong cohorts
  geom_abline(intercept = -1968, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 28.7, label = "1968", color = "red") +
  geom_abline(intercept = -1986, slope = 1, color = 'red') +
  annotate("text", x = 1995, y = 10.7, label = "1986", color = "red") +
  geom_abline(intercept = -1998, slope = 1, color = 'red') +
  annotate("text", x = 1998, y = 1.7, label = "1998", color = "red") +
  geom_abline(intercept = -2010, slope = 1, color = 'red') +
  annotate("text", x = 2010, y = 1.7, label = "2010?", color = "red")

ye_age_all
