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
age <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                                    ling2009, ling2010, ling2011, ling2012, ling2013, 
                                    ling2014, ling2015, ling2016, ling2017, ling2018, 
                                    ling2019, ling2020, ling2021, ling2022))
detach(package:plyr)


agecomp <- age %>% 
  filter(
    !(PORT %in% c('CCI', 'Cordova')), 
    YEAR >= 1996, 
    !is.na(AGE),
    !(AGE %in% c("I", "")) # Not sure what is up with these values for age
    ) %>% 
  area_split_sf() 

agecomp$AGE <- as.numeric(agecomp$AGE) # Weird values for age made them characters

# Sort the data by port, year, and user 
agecomp <- agecomp %>% 
  arrange(SFmgmtarea, USER, YEAR, AGE)

# Perform frequency analysis
comp <- agecomp %>%
  mutate(
    USER = case_when(
      USER == "P" ~ "Private",
      USER == "C" ~ "Charter",
      USER == "" ~ "Unknown",
      TRUE ~ USER
    )
  ) %>% 
  group_by(SFmgmtarea, USER, YEAR, AGE) %>%
  summarise(COUNT = n()) %>%
  mutate(PERCENT = COUNT / sum(COUNT) * 100)

# Perform means analysis
means <- agecomp %>%
  group_by(SFmgmtarea, YEAR) %>%
  summarise(MEAN_AGE = mean(AGE))
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
  select(-USER, -COUNT, -PERCENT, AGE)

# Sort the comp data frame by port, year, and age
comp <- comp %>% 
  arrange(SFmgmtarea, YEAR, AGE)

# Calculate the sum of nijC, nijP, nijU, and nijM by port, year, and age
comp2 <- comp %>%
  group_by(SFmgmtarea, YEAR, AGE) %>%
  summarise(nijC = sum(nijC),
            nijP = sum(nijP),
            nijU = sum(nijU),
            nijM = sum(nijM))

# Replace missing values with 0 in comp2
comp2 <- comp2 %>%
  mutate(across(starts_with("nij"), ~ if_else(is.na(.), 0, .)))

##Obtain and merge total lc sample size for each user group
# Calculate the sums of nijC, nijP, nijU, nijM by port and year
totaln <- comp2 %>%
  group_by(SFmgmtarea, YEAR) %>%
  summarise(niC = sum(nijC),
            niP = sum(nijP),
            niU = sum(nijU),
            niM = sum(nijM))

# Merge comp2 and totaln data frames by port and year
comp3 <- merge(comp2, totaln, by = c("SFmgmtarea", "YEAR")) %>% 
# Calculate pijC, vpijC, pijP, vpijP
  mutate(pijC = nijC / niC,
         vpijC = pijC * (1 - pijC) / (niC - 1),
         pijP = nijP / niP,
         vpijP = pijP * (1 - pijP) / (niP - 1))

##obtain proportion of harvest by species externally
get_data("data/Harvest/LC/")

pharv <- harvbyport %>%
  mutate(
  SFmgmtarea = case_when(
    Port == 'Homer' ~ 'CI',
    Port == ' Kodiak' ~ 'KOD',
    Port %in% c('Whittier', 'Valdez') ~ 'PWS',
    Port == 'Seward' ~ 'NG'
  )
)
#Estimate age comp
# Merge comp3 and pharv data frames by Mgmtarea and year

comp4 <- merge(comp3, pharv, by = c("SFmgmtarea", "YEAR")) %>% 
# Calculate HijC, vHijC, SEHijC, HijP, vHijP, SEHijP, Hij, SEHij, pij, vpij, SEpij for specific conditions
  mutate(
    n = case_when(
      SFmgmtarea %in% c("CI", "PWS") ~ niC + niP,
      (SFmgmtarea == "NG" & YEAR >= 2001) ~ niC + niP,
      SFmgmtarea == "KOD" ~ niC + niP
    ),
    HijC = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ pijC * HC
    ),
    vHijC = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ (pijC^2 * vHC) + (vpijC * HC^2) - (vpijC * vHC)
    ),
    SEHijC = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ sqrt(vHijC)
    ),
    HijP = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ pijP * HP
    ),
    vHijP = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ (pijP^2 * vHP) + (vpijP * HP^2) - (vpijP * vHP)
    ),
    SEHijP = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ sqrt(vHijP)
    ),
    Hij = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ HijC + HijP
    ),
    SEHij = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ sqrt(vHijC + vHijP)
    ),
    pij = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ Hij / H
    ),
    vpij = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ (1 / H^2) * (vHC * (pijC * HP - HijP)^2 / H^2 + vHP * (pijP * HC - HijC)^2 / H^2 + vpijC * HC^2 + vpijP * HP^2)
    ),
    SEpij = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ sqrt(vpij)
    ), #Calculations for Seward 1996-2000
    n = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ niC + niP + niU + niM,
      TRUE ~ n
    ),
    pij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ (nijC + nijP + nijU + nijM) / (niC + niP + niU + niM),
      TRUE ~ pij
    ),
    vpij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ pij * (1 - pij) / ((niC + niP + niU + niM) - 1),
      TRUE ~ vpij
    ),
    SEpij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sqrt(vpij),
      TRUE ~ SEpij
    ),
    Hij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ pij * H,
      TRUE ~ Hij
    ),
    vHij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ (pij^2 * vH) + (vpij * H^2) - (vpij * vH)
    ),
    SEHij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sqrt(vHij),
      TRUE ~ SEHij
    )
  ) %>%
  select(-vHijC, -vHijP, -vpij, -vHij) %>%
  select(SFmgmtarea, YEAR, AGE, n, nijC, nijP, nijU, nijM, pijC, vpijC, pijP, vpijP, HijC, HijP, Hij, SEHij, pij, SEpij) %>%
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
  facet_wrap(~SFmgmtarea, nrow = 1) +
  theme_minimal() +
  labs(title = "Lingcod Age Compositions",
       x = "Year",
       y = "Age",
       y2 = "Sample Size") +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  theme()
