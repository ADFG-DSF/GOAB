###################################################################################################
# Age comp uses same formulas as species comp (only summarize for fish of known species). 
# 
# For Seward 1996-2000, age comp estimated from pooled data because there were four user groups 
# in the raw data (charter, private, Army, USAF) but only two user groups in the SWHS estimates.
# 
# Likewide, the Kodiak 2013 age comp was estimated from raw proportions because the tech did not
# obtain otoliths from charter-caught rockfish until late in the season. Therefore, weighted 
# estimates put too much influence on a few () charter-caught fish that do not reflect the 
# age composition, As a result, age comp is inconsistent with past estimates and relatively
# strong and weak cohorts. 
# 
# All other ports and years use stratified estimates described in the report.
#
# Translated from SAS to R by CWM 06/13/23
###################################################################################################

library(tidyverse)


source("functions.R")

#Function call
get_data("data/RF/")


#####
library(plyr)

agecomp <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                     rock2004, rock2005, rock2006, rock2007, rock2008, 
                                     rock2009, rock2010, rock2011, rock2012, rock2013, 
                                     rock2014, rock2015, rock2016, rock2017, rock2018, 
                                     rock2019, rock2020, rock2021, rock2022))

detach(package:plyr)
# Filter rows with non-missing age
agecomp <- agecomp %>%
  filter(!is.na(AGE)) %>%
  # Filter species without ages
  filter(!(SP %in% c(144, 168, 169))) %>% 
# Rename specific species
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
  )) %>%
  area_split_sf() %>% 
  filter (YEAR >= 1996) %>%
  arrange(SFmgmtarea, USER, YEAR, SPECIES, AGE)

##Boxplot showing range of ages by sex

# Filter rows with non-empty sex
bysex <- agecomp %>%
  filter(SEX != "")

# Create the panel plot
boxplot <- ggplot(data = bysex) +
  geom_violin(aes(x = AGE, y = SEX), trim = FALSE, scale = "width") +
  facet_wrap(~ SFmgmtarea, ncol = 5) +
  labs(x = "Age", y = "Density", title = "Age Distribution by Sex") +
  theme_bw()

# Save the plot as PDF file
# pdf("O:/DSF/GOAB/R Code/Figures/rfbysex_boxplot_mgmtarea.pdf")
boxplot
# dev.off()

##Start with raw age freq of each species by port, user, year

# Calculate frequency of age by port, user, year, species
comp <- agecomp %>%
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
  select(-COUNT) %>%
  arrange(SFmgmtarea, YEAR, SPECIES, AGE)

# Calculate sums of nijC, nijP, nijU, nijM
comp2 <- comp %>%
  group_by(SFmgmtarea, YEAR, SPECIES, AGE) %>%
  summarise(nijC = sum(nijC, na.rm = TRUE),
            nijP = sum(nijP, na.rm = TRUE),
            nijU = sum(nijU, na.rm = TRUE),
            nijM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

##Obtain and merge total rf sample size for each user group
# Calculate sums of nijC, nijP, nijU, nijM for each combination of port, year, and species
totaln <- comp2 %>%
  group_by(SFmgmtarea, YEAR, SPECIES) %>%
  summarise(niC = sum(nijC, na.rm = TRUE),
            niP = sum(nijP, na.rm = TRUE),
            niU = sum(nijU, na.rm = TRUE),
            niM = sum(nijM, na.rm = TRUE)) %>%
  ungroup()

# Merge comp2 and totaln data frames
comp3 <- merge(comp2, totaln, by = c("SFmgmtarea", "YEAR", "SPECIES")) %>%
  mutate(pijC = nijC / niC,
         vpijC = pijC * (1 - pijC) / (niC - 1),
         pijP = nijP / niP,
         vpijP = pijP * (1 - pijP) / (niP - 1))


##obtain proportion of harvest by species externally, file created by RFspcomp9616
get_data("data/Harvest/RF/")
#####



pharv <- harvbyspecies %>% 
  mutate(SPECIES = case_when(
    species == "Blackgll" ~ "Blackgill",
    species == "Quill" ~ "Quillback",
    species == "Redstrpe" ~ "Redstripe",
    species == "Harleq" ~ "Harlequin",
    species == "Rosethrn" ~ "Rosethorn",
    species == "Shortrkr" ~ "Shortraker",
    species == "Shrpchin" ~ "Sharpchin",
    species == "Silvergr" ~ "Silvergray",
    species == "Vermilon" ~ "Vermilion",
    species == "Yelleye" ~ "Yelloweye",
    species == "Yelltail" ~ "Yellowtail",
    species == "DuskyDark" ~ "DuskyDark",
    TRUE ~ species
  )) %>% 
# Calculate vHiC, vHi, vHiP
  mutate(vHiC = SEHiC^2,
         vHi = SEHi^2,
         vHiP = SEHiP^2,
         SFmgmtarea = case_when(
           PORT == "Kodiak" ~ "KOD",
           PORT %in% c('Whittier', 'Valdez', 'Cordova') ~ 'PWS',
           PORT == 'Seward' ~ 'NG',
           PORT %in% c('Homer', 'CI', 'CCI') ~ 'CI'
         )) %>% 
  arrange(SFmgmtarea, YEAR, SPECIES)

##Merge harvest by species and estimate age comp
comp4 <- merge(comp3, pharv, by = c("SFmgmtarea", "YEAR", "SPECIES")) %>%
  mutate(
    n = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                 (SFmgmtarea == 'NG' & YEAR >= 2001) |
                 (SFmgmtarea == 'KOD' & YEAR != 2013), niC + niP, NA),
    HijC = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                    (SFmgmtarea == 'NG' & YEAR >= 2001) |
                    (SFmgmtarea == 'KOD' & YEAR != 2013), pijC * HiC, NA),
    vHijC = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                     (SFmgmtarea == 'NG' & YEAR >= 2001) |
                     (SFmgmtarea == 'KOD' & YEAR != 2013),
                   pijC^2 * vHiC + vpijC * HiC^2 - vpijC * vHiC, NA),
    SEHijC = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                      (SFmgmtarea == 'NG' & YEAR >= 2001) |
                      (SFmgmtarea == 'KOD' & YEAR != 2013), sqrt(vHijC), NA),
    HijP = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                    (SFmgmtarea == 'NG' & YEAR >= 2001) |
                    (SFmgmtarea == 'KOD' & YEAR != 2013), pijP * HiP, NA),
    vHijP = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                     (SFmgmtarea == 'NG' & YEAR >= 2001) |
                     (SFmgmtarea == 'KOD' & YEAR != 2013),
                   pijP^2 * vHiP + vpijP * HiP^2 - vpijP * vHiP, NA),
    SEHijP = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                      (SFmgmtarea == 'NG' & YEAR >= 2001) |
                      (SFmgmtarea == 'KOD' & YEAR != 2013), sqrt(vHijP), NA),
    Hij = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                   (SFmgmtarea == 'NG' & YEAR >= 2001) |
                   (SFmgmtarea == 'KOD' & YEAR != 2013), HijC + HijP, NA),
    SEHij = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                     (SFmgmtarea == 'NG' & YEAR >= 2001) |
                     (SFmgmtarea == 'KOD' & YEAR != 2013), sqrt(vHijC + vHijP), NA),
    pij = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                   (SFmgmtarea == 'NG' & YEAR >= 2001) |
                   (SFmgmtarea == 'KOD' & YEAR != 2013), Hij / Hi, NA),
    vpij = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                    (SFmgmtarea == 'NG' & YEAR >= 2001) |
                    (SFmgmtarea == 'KOD' & YEAR != 2013),
                  (1 / Hi^2) * (vHiC * (pijC * HiP - HijP)^2 / Hi^2 +
                                  vHiP * (pijP * HiC - HijC)^2 / Hi^2 +
                                  vpijC * HiC^2 + vpijP * HiP^2), NA),
    SEpij = ifelse(SFmgmtarea == 'CI' | SFmgmtarea == 'PWS' |
                     (SFmgmtarea == 'NG' & YEAR >= 2001) |
                     (SFmgmtarea == 'KOD' & YEAR != 2013), sqrt(vpij), NA)
  )  %>%
  mutate(
    n = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                 (SFmgmtarea == 'KOD' & YEAR == 2013),
               niC + niP + niU + niM, n),
    pij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                   (SFmgmtarea == 'KOD' & YEAR == 2013),
                 (nijC + nijP + nijU + nijM) / (niC + niP + niU + niM), pij),
    vpij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                    (SFmgmtarea == 'KOD' & YEAR == 2013),
                  pij * (1 - pij) / ((niC + niP + niU + niM) - 1), vpij),
    SEpij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                     (SFmgmtarea == 'KOD' & YEAR == 2013), sqrt(vpij), SEpij),
    Hij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                   (SFmgmtarea == 'KOD' & YEAR == 2013), pij * Hi, Hij),
    vHij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                    (SFmgmtarea == 'KOD' & YEAR == 2013),
                  (pij^2 * vHi) + (vpij * Hi^2) - (vpij * vHi), 0),
    SEHij = ifelse((SFmgmtarea == 'NG' & YEAR >= 1996 & YEAR <= 2000) |
                     (SFmgmtarea == 'KOD' & YEAR == 2013), sqrt(vHij), SEHij)
  ) %>%
  filter(!(SFmgmtarea == "Whittier" & YEAR >= 1996 & YEAR <= 1998)) %>%
  select(-c(vHijC, vHijP, vpij, vHij))

##Primary species estimates - reduced and formatted
##Primary species identified by looking at sample sizes and iteratively making bubble plots
# to see information content. If sample sizes consistently below about 50 I didn't consider primary.
# 	However, if recent sample sizes are high enough to get a picture of the harvest age structure,
# 	they were included.

primarycomp <- comp4 %>%
  filter(
    SPECIES == "Black" |
      (SPECIES == "Yelloweye" & !(PORT %in% c("CCI", "Kodiak"))) |
      (SPECIES == "Dark" & PORT == "CI") |
      (SPECIES == "Dusky" & PORT %in% c("CI", "Kodiak")) |
      (SPECIES == "Copper" & PORT == "Valdez") |
      (SPECIES == "Quillback" & PORT %in% c("Seward", "Valdez", "Whittier"))
  )

##Create tables for appendix
plotages <- primarycomp %>%
  mutate(Cohort = YEAR - AGE) %>%
  mutate(Cohort = ifelse(Cohort %in% c(1979, 1991, 1996, 2002), NA, Cohort))
#Black rockfish
black <- plotages %>%
  filter(species == 'Black')

scale_b <- max(black$n) / max(black$AGE)

ggplot(data = black) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
 # scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps')

#Yelloweye rockfish
ye <- plotages %>%
  filter(species == 'Yelloweye')

scale_y <- max(ye$n) / max(ye$AGE)

ggplot(data = ye) +
  geom_point(aes(x = YEAR, y = AGE, linewidth = pij, fill = pij), shape = 21, color = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps')

#Dark rockfish
Dark <- plotages %>%
  filter(species == 'Dark')

scale_d <- max(Dark$n) / max(Dark$AGE)

ggplot(data = Dark) +
  geom_point(aes(x = YEAR, y = AGE, linewidth = pij, fill = pij), shape = 21, color = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_d), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_d, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Dark Rockfish Age Comps')

#Dusky rockfish
Dusky <- plotages %>%
  filter(species == 'Dusky')

scale_du <- max(Dusky$n) / max(Dusky$AGE)

ggplot(data = Dusky) +
  geom_point(aes(x = YEAR, y = AGE, linewidth = pij, fill = pij), shape = 21, color = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_du), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_du, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Dusky Rockfish Age Comps')

#Copper rockfish
Copper <- plotages %>%
  filter(species == 'Copper')

scale_c <- max(Copper$n) / max(Copper$AGE)

ggplot(data = Copper) +
  geom_point(aes(x = YEAR, y = AGE, linewidth = pij, fill = pij), shape = 21, color = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_c), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_c, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Copper Rockfish Age Comps')

#Quillback rockfish
Quillback <- plotages %>%
  filter(species == 'Quillback')

scale_q <- max(Quillback$n) / max(Quillback$AGE)

ggplot(data = Quillback) +
  geom_point(aes(x = YEAR, y = AGE, linewidth = pij, fill = pij), shape = 21, color = 'gray', stroke = 0.01, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_q), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_q, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  facet_wrap(~ SFmgmtarea) +
  scale_x_continuous(breaks = seq(1995, 2020, 5), labels = as.character(seq(1995, 2020, 5)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age') +
  scale_size_continuous(range = c(0.1, 2), guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Quillback Rockfish Age Comps')

##hbarparm plots of age comp for primare species
##First break up datasets by species

BlackAge <- primarycomp %>%
  filter(SPECIES == 'Black')

YEAge <- primarycomp %>%
  filter(SPECIES == 'Yelloweye')

DuskyAge <- primarycomp %>%
  filter(SPECIES == 'Dusky')

DarkAge <- primarycomp %>%
  filter(SPECIES == 'Dark')

QuillAge <- primarycomp %>%
  filter(SPECIES == 'Quillback')

CopperAge <- primarycomp %>%
  filter(SPECIES == 'Copper')

#Plot proportion of harvest by age
  ggplot(data = BlackAge) +
    geom_bar(aes(x = AGE), fill = "lightgray") +
    facet_wrap(~ SFmgmtarea, ncol = 5) +
    labs(title = "Black Rockfish Relative Proportion of Harvest", x = "Age", y = "pij") +
    theme_bw()
  
  ggplot(data = YEAge) +
    geom_bar(aes(x = AGE), fill = "lightgray") +
    facet_wrap(~ SFmgmtarea, ncol = 5) +
    labs(title = "Yelloweye Rockfish Ages", x = "Age", y = "pij") +
    theme_bw()

  ##age frequency of each species by port, sex, year (pooled by user)
  # Filter the data for Black species
  black <- agecomp %>%
    filter(SP == 142 & SEX != '' & !is.na(AGE)) %>%
    arrange(SFmgmtarea, YEAR, SEX)
  
  # Calculate sample size by port and year
  sampn <- black %>%
    group_by(SFmgmtarea, YEAR) %>%
    summarise(NAged = n()) %>%
    ungroup()
##Next get age FREQUENCY and calculate proportion of total by age and sex
  # Calculate frequencies by port, year, and sex
  agecompsex <- black %>%
    group_by(AGE, SFmgmtarea, YEAR, SEX) %>%
    summarise(freq = n()) %>%
    ungroup()
  
  # Merge with sample size data
  agecompsex2 <- sampn %>%
    inner_join(agecompsex, by = c("SFmgmtarea", "YEAR")) %>%
    mutate(pi = freq / NAged,
           SEpi = sqrt(pi * (1 - pi) / (NAged - 1)),
           jyear = ifelse(SEX == "F", YEAR - 0.1, YEAR + 0.1))
  
  # Plot bubble chart
  ggplot(agecompsex2, aes(x = jyear, y = AGE, size = pi, fill = SEX)) +
    geom_point(shape = 21, color = "black") +
    scale_size_continuous(range = c(1, 6)) +
    scale_fill_manual(values = c("F" = "lightgray", "M" = "lightblue")) +
    facet_wrap(~ SFmgmtarea, ncol = 1) +
    labs(x = "Year", y = "Age") +
    theme_bw() +
    theme(panel.spacing = unit(0.5, "lines"),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
    guides(fill = guide_legend(title = "Sex", nrow = 1)) +
    coord_cartesian(xlim = c(1996, 2016), ylim = c(0, 60)) +
    scale_x_continuous(breaks = seq(1996, 2016, 1), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(0, 60, 5), minor_breaks = NULL) +
    geom_hline(yintercept = seq(0, 60, 5), color = "gray", linetype = "dashed") +
    geom_vline(xintercept = seq(1996, 2016, 1), color = "gray", linetype = "dashed")
  