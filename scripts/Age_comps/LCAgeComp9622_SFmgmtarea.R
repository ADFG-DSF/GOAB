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

library(plyr)
library(tidyverse)

get_data <- function(a) {
  
  
  print(a)
  
  files <- list.files(path=a,
                      pattern="*.csv", full.names=F, recursive=FALSE)
  
  
  for(i in seq(1, length(files))) {
    print(files[[i]])
    n <- gsub(".csv", "",files[[i]])
    assign(n, read.csv(paste0(a,files[[i]])), envir = .GlobalEnv)
    
  }
  
}

#Function call
get_data("O:/DSF/GOAB/R data/LC/")

library(plyr)
agecomp <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                                    ling2009, ling2010, ling2011, ling2012, ling2013, 
                                    ling2014, ling2015, ling2016, ling2017, ling2018, 
                                    ling2019, ling2020, ling2021, ling2022))
detach(package:plyr)


agecomp <- agecomp %>% filter(!(PORT %in% c('CCI', 'Cordova')), YEAR >= 1996, !is.na(AGE)) %>% 
  mutate(SFmgmtarea = case_when(
    STATAREA > 440000 & STATAREA < 480000 | STATAREA %in% c(485430, 485500, 485530, 485600,
                                                            485630, 485700, 485730, 485800, 485831, 485901, 485931, 485932, 485935,
                                                            486001, 486002, 486003, 486004, 486005, 486031, 486032, 486033, 486034,
                                                            486100) ~ 'PWS',
    STATAREA %in% c(485832, 485902, 485933, 485934, 485935, 486002, 495831, 495901, 495902,
                    495931, 495932, 495933, 495934, 495935, 495936, 495937, 495938, 495939, 496001, 496002,
                    505831, 505901, 505902, 505903, 505904, 505905, 505906, 505907, 505908, 505909, 505931, 505932,
                    505933, 505934) ~ 'NG',
    STATAREA %in% c(495800, 495832, 505700, 505730, 505800, 505832, 515630, 515700, 515730,
                    515801, 515802, 515833, 525600, 525630, 525701, 525702, 525703, 525731, 525732, 525733,
                    525801, 525802, 525803, 525804, 525805, 525806, 525807, 525832, 525833, 525834, 535601,
                    535602, 535631, 535632, 535633, 535634, 535701, 535702, 535703, 535704, 535705, 535706,
                    535707, 535731, 535732, 535733, 535734, 535802, 535803, 535831, 545601, 545602, 545631,
                    545632, 545633, 545701, 545702, 545703, 545704, 545732, 545733, 545734, 545804, 555630,
                    555701, 555733) ~ 'KOD',
    STATAREA %in% c(555731, 555732, 545731, 545801, 545802, 545803, 535801, 535832) ~ 'AKPen',
    STATAREA %in% c(515831, 515832, 515901, 515902, 515903, 515904, 515905, 515906, 515907,
                    515908, 515931, 515932, 515933, 515934, 515935, 515936, 515937, 515938, 515939,
                    516001, 516002, 525831, 525835, 525836, 525837, 525901, 525902, 525931, 525932,
                    526002, 526003, 535833, 535834, 535901, 535902, 535903, 535904, 535905, 535906,
                    535931, 535932, 535933, 545900) ~ 'CI',
  ),
  SFmgmtarea = (case_when(
    PORT %in% c('Homer', 'CCI') ~ 'CI',
    PORT == 'Kodiak' ~ 'KOD',
    PORT %in% c('Whittier', 'Valdez', 'Cordova') ~ 'PWS',
    PORT == 'Seward' ~ 'NG',
    TRUE ~ PORT
  ))) %>% filter (YEAR >= 1996)

# Sort the data by port, year, and user 
agecomp <- agecomp %>% 
  arrange(SFmgmtarea, USER, YEAR, AGE)

# Perform frequency analysis
comp <- agecomp %>%
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
comp3 <- merge(comp2, totaln, by = c("SFmgmtarea", "YEAR"))


# Calculate pijC, vpijC, pijP, vpijP
comp3 <- comp3 %>%
  mutate(pijC = nijC / niC,
         vpijC = pijC * (1 - pijC) / (niC - 1),
         pijP = nijP / niP,
         vpijP = pijP * (1 - pijP) / (niP - 1))

##obtain proportion of harvest by species externally
get_data("O:/DSF/GOAB/R data/Harvest/LC/")

pharv <- harvbyport


pharv <- pharv %>% mutate(
  SFmgmtarea = case_when(
    Port == 'Homer' ~ 'CI',
    Port == ' Kodiak' ~ 'KOD',
    Port %in% c('Whittier', 'Valdez') ~ 'PWS',
    Port == 'Seward' ~ 'NG'
  )
)
#Estimate age comp
# Merge comp3 and pharv data frames by Mgmtarea and year

comp4 <- merge(comp3, pharv, by = c("SFmgmtarea", "YEAR"))

# Calculate HijC, vHijC, SEHijC, HijP, vHijP, SEHijP, Hij, SEHij, pij, vpij, SEpij for specific conditions
comp4 <- comp4 %>%
  mutate(
    n = case_when(
      SFmgmtarea %in% c("CI", "PWS") |
        (SFmgmtarea == "NG" & YEAR >= 2001) |
        SFmgmtarea == "KOD" ~ sum(niC, niP)
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
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sum(niC, niP, niU, niM)
    ),
    pij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sum(nijC, nijP, nijU, nijM) / sum(niC, niP, niU, niM)
    ),
    vpij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ pij * (1 - pij) / (sum(niC, niP, niU, niM) - 1)
    ),
    SEpij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sqrt(vpij)
    ),
    Hij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ pij * H
    ),
    vHij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ (pij^2 * vH) + (vpij * H^2) - (vpij * vH)
    ),
    SEHij = case_when(
      SFmgmtarea == "NG" & YEAR >= 1996 & YEAR <= 2000 ~ sqrt(vHij)
    )
  ) %>%
  select(-vHijC, -vHijP, -vpij, -vHij)




# Format columns
comp4 %>%
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

ggplot(data = comp4) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), fill = "lightgray", shape = 21, color = "black") +
  #geom_line(aes(x = YEAR, y = n), color = "gray", size = 3, alpha = 0.7) +
  scale_size_continuous(range = c(0.1, 2)) +
  facet_wrap(~SFmgmtarea, nrow = 1) +
  theme_minimal() +
  labs(title = "Lingcod Age Compositions",
       x = "Year",
       y = "Age",
       y2 = "Sample Size") +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme(panel.grid = element_blank())
