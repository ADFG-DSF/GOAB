#############################################################################
# Create bubble plots of LC ages
# By Mgmt area
#
# Created by CWM on 06/13/24
#############################################################################

library(tidyverse)


source("functions.R")

#Function call
get_data("data/LC/")

#####
library(plyr)

lc_all <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                                   ling2009, ling2010, ling2011, ling2012, ling2013, 
                                   ling2014, ling2015, ling2016, ling2017, ling2018, 
                                   ling2019, ling2020, ling2021, ling2022, ling2023))
detach(package:plyr)

lc_all <- lc_all %>% mutate(
  USER = case_when(
    USER == 'P' ~ 'Private',
    USER == 'C' | USER == 'SewMilC' ~ 'Charter',
    TRUE ~ USER
  )
)

lc_all$AGE <- as.integer(lc_all$AGE)


###############################
# Select Mgmt Area
###############################
lc_pws <- area_split_sf(lc_all) %>% 
  filter(SFmgmtarea == "PWS") %>% 
  select(-X) 

######### Age ################

# Filter rows with non-missing age
agecomp <- lc_pws %>%
  filter(!is.na(AGE))%>%
  mutate(
    PORT = (case_when(
      PORT %in% c('Homer', 'CCI') ~ 'CI',
      TRUE ~ PORT
    ))) %>% filter (YEAR >= 1996, !is.na(AGE))



##########################################33
# Bubble Plot
##############################################
#AGE
comp_age <- agecomp %>%
  group_by(SFmgmtarea, USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  ungroup()

##restructure data file so sample size (nj) by each user group is a separate variable,
##all on one line for each species

# Assign values to respective variables based on user value
comp_age <- comp_age %>%
  mutate(nijC = if_else(USER == 'Charter', COUNT, 0),
         nijP = if_else(USER == 'Private', COUNT, 0),
         nijU = if_else(USER == 'Unknown', COUNT, 0),
         nijM = if_else(USER == 'SewMilC', COUNT, 0)) %>%
  select(-COUNT)


# Sort the dataframe
comp_age <- comp_age %>%
  arrange(SFmgmtarea, YEAR, AGE)

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
##Create tables for appendix
plotages <- comp3_age %>%
  mutate(Cohort = YEAR - AGE) %>%
  mutate(Cohort = ifelse(Cohort %in% c(1979, 1991, 1996, 2002), NA, Cohort))

# Bubble Plot
scale <- max(plotages$n) / max(plotages$AGE)


lc_age <- ggplot(data = plotages %>% filter(YEAR < 2023)) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2022, 3), labels = as.character(seq(1995, 2022, 3)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale, name = 'Sample Size', breaks = seq(0, 700, 100))) +  # Add a second Y-axis for 'n'
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

lc_age

