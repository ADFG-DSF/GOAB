################################################################################
# Age comp uses same formulas as species comp (only summarize for fish of known species). 
# 
# For Seward 1996-2000, age comp estimated from pooled data because there were four user groups 
# in the raw data (charter, private, Army, USAF) but only two user groups in the SWHS estimates.
# 
# All other ports and years use stratified estimates described in the report.
#
# Translated from SAS to R by CWM 06/13/23
###############################################################################

library(tidyverse)


source("functions.R")

#Function call
get_data("data/LC/")

library(plyr)

agecomp <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                                    ling2009, ling2010, ling2011, ling2012, ling2013, 
                                    ling2014, ling2015, ling2016, ling2017, ling2018, 
                                    ling2019, ling2020, ling2021, ling2022))
detach(package:plyr)


agecomp <- agecomp %>% 
  filter(
    !(PORT %in% c('CCI', 'Cordova')), 
    YEAR >= 1996, 
    !is.na(AGE),
    !(AGE %in% c("I", "")) # Not sure what is up with these values for age
    )

agecomp$AGE <- as.numeric(agecomp$AGE) # Weird values for age made them characters


# Perform frequency analysis
comp <- agecomp %>%
  group_by(PORT, USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  mutate(PERCENT = COUNT / sum(COUNT) * 100)

# Perform means analysis
means <- agecomp %>%
  group_by(PORT, YEAR) %>%
  summarise(MEAN_AGE = mean(AGE, na.rm = TRUE))
##restructure data file so sample size (nj) by each user group is a separate variable,
##all on one line for each species
# Update the comp data frame based on user values
comp <- comp %>%
  mutate(
    nijC = if_else(USER == 'Charter', COUNT, 0),
    nijP = if_else(USER == 'Private', COUNT, 0),
    nijU = if_else(USER == 'Unknown', COUNT, 0),
    nijM = if_else(USER == 'SewMilC', COUNT, 0)
  ) %>%
  select(-USER, -COUNT, -PERCENT, AGE) %>% 
  arrange(PORT, YEAR, AGE)

# Calculate the sum of nijC, nijP, nijU, and nijM by port, year, and age
comp2 <- comp %>%
  group_by(PORT, YEAR, AGE) %>%
  summarise(nijC = sum(nijC),
            nijP = sum(nijP),
            nijU = sum(nijU),
            nijM = sum(nijM)) %>% 
# Replace missing values with 0 in comp2
  mutate(across(starts_with("nij"), ~ if_else(is.na(.), 0, .)))

##Obtain and merge total lc sample size for each user group
# Calculate the sums of nijC, nijP, nijU, nijM by port and year
totaln <- comp2 %>%
  group_by(PORT, YEAR) %>%
  summarise(niC = sum(nijC),
            niP = sum(nijP),
            niU = sum(nijU),
            niM = sum(nijM))

# Merge comp2 and totaln data frames by port and year
comp3 <- merge(comp2, totaln, by = c("PORT", "YEAR")) %>% 
# Calculate pijC, vpijC, pijP, vpijP
  mutate(pijC = nijC / niC,
         vpijC = pijC * (1 - pijC) / (niC - 1),
         pijP = nijP / niP,
         vpijP = pijP * (1 - pijP) / (niP - 1))

##obtain proportion of harvest by species externally
get_data("data/Harvest/LC/")

pharv <- harvbyport 
pharv <-  rename(pharv, PORT = Port)

#Estimate age comp
# Merge comp3 and pharv data frames by port and year
comp4 <- merge(comp3, pharv, by = c("PORT", "YEAR")) %>% 
# Calculate HijC, vHijC, SEHijC, HijP, vHijP, SEHijP, Hij, SEHij, pij, vpij, SEpij for specific conditions
  mutate(n = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                    niC + niP,
                    niC + niP + niU + niM),
         HijC = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                       pijC * HC,
                       0),
         vHijC = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                        (pijC^2 * vHC) + (vpijC * HC^2) - (vpijC * vHC),
                        0),
         SEHijC = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                         sqrt(vHijC),
                         0),
         HijP = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                       pijP * HP,
                       0),
         vHijP = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                        (pijP^2 * vHP) + (vpijP * HP^2) - (vpijP * vHP),
                        0),
         SEHijP = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                         sqrt(vHijP),
                         0),
         Hij = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                      HijC + HijP,
                      0),
         SEHij = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                        sqrt(vHijC + vHijP),
                        0),
         pij = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                      Hij / H,
                      0),
         vpij = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                       (1 / H^2) * (vHC * (pijC*HP-HijP)^2/H^2 + vHP*(pijP*HC-HijC)^2/H^2 + vpijC*H^2 + vpijP*H^2),
                       0),
         SEpij = ifelse(PORT %in% c("Homer", "Valdez", "Whittier", "Kodiak") | (PORT == "Seward" & YEAR >= 2001),
                        sqrt(vpij),
                        0)) %>%
  mutate(
    n = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000, niC + niP + niU + niM, n),
    pij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                 (nijC + nijP + nijU + nijM) / (niC + niP + niU + niM),
                 pij),
    vpij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                  pij * (1 - pij) / (niC + niP + niU + niM - 1),
                  vpij),
    SEpij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                   sqrt(vpij),
                   SEpij),
    Hij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                 pij * H,
                 Hij),
    vHij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                  (pij^2 * vH) + (vpij * H^2) - (vpij * vH),
                  0),
    SEHij = ifelse(PORT == "Seward" & YEAR >= 1996 & YEAR <= 2000,
                   sqrt(vHij),
                   SEHij)
  ) %>%
  select(PORT, YEAR, AGE, n, nijC, nijP, nijU, nijM, pijC, vpijC, pijP, vpijP, HijC, HijP, Hij, SEHij, pij, SEpij) %>%
  mutate(
    pijC = format(pijC, digits = 3),
    vpijC = format(vpijC, digits = 4),
    pijP = format(pijP, digits = 3),
    vpijP = format(vpijP, digits = 4),
    #HijC = format(HijC, digits = 0),
    #HijP = format(HijP, digits = 0),
    #Hij = format(Hij, digits = 0),
    SEHij = format(SEHij, digits = 1),
    pij = format(pij, digits = 3),
    SEpij = format(SEpij, digits = 4)
  )                             


plotages <- comp4 %>%
  mutate(Cohort = YEAR - AGE) %>%
  mutate(Cohort = if_else(Cohort %in% c(1979, 1991, 1996, 2002), NA_real_, Cohort))

##sgplot bubble plots for publication

scale <- max(plotages$n) / max(plotages$AGE)

ggplot(data = comp4) +
  geom_point(aes(x = YEAR, y = AGE, size = as.numeric(pij)), fill = "lightgray", shape = 21, color = "black") +
  geom_line(aes(x = YEAR, y = n / scale), color = "gray", size = 1.4, alpha = 0.7) +
 # scale_size_continuous(range = c(0.1, 2)) +
  scale_y_continuous(breaks = seq(0, 30, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale, name = 'Sample Size', breaks = seq(0, 700, 100))) +
  facet_wrap(~PORT, nrow = 1) +
  theme_minimal() +
  labs(title = "Lingcod Age Compositions",
       x = "Year",
       y = "Age",
       y2 = "Sample Size") +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  theme()

