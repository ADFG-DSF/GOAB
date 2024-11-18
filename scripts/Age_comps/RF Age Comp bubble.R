#############################################################################
# Create bubble plots of RF ages
# By Mgmt area
#
# Created by CWM on 01/25/24
#############################################################################
library(tidyverse)

##############################################################################
# Functions
##############################################################################

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

area_split <- function(a) {
  a %>% 
    mutate(
      SPECIES = case_when(
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
      SFmgmtarea = case_when(
        STATAREA >= 440000 & STATAREA < 480000 | STATAREA %in% c(485430, 485500, 485530, 485600,
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
        TRUE ~ 'ZZZ'
      ),
      SFmgmtarea = case_when(
        is.na(SFmgmtarea) | SFmgmtarea == 'ZZZ' ~ case_when(
          PORT %in% c('Homer', 'CCI') ~ 'CI',
          PORT == 'Kodiak' ~ 'KOD',
          PORT %in% c('Whittier', 'Valdez', 'Cordova') ~ 'PWS',
          PORT == 'Seward' ~ 'NG',
          TRUE ~ PORT
        ),
        TRUE ~ SFmgmtarea
      )
    )
}

#Function call
get_data("O:/DSF/GOAB/R data/RF/")

#####
library(plyr)

rf_all <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                   rock2004, rock2005, rock2006, rock2007, rock2008, 
                                   rock2009, rock2010, rock2011, rock2012, rock2013, 
                                   rock2014, rock2015, rock2016, rock2017, rock2018, 
                                   rock2019, rock2020, rock2021, rock2022, rock2023))
detach(package:plyr)

##########
# Select management area
########
rf_pws <- area_split(rf_all) %>% 
  filter(SFmgmtarea == "PWS")


# Filter rows with non-missing age
agecomp <- rf_pws %>%
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
##Create tables for appendix
plotages <- comp3_age %>%
  mutate(Cohort = YEAR - AGE) %>%
  mutate(Cohort = ifelse(Cohort %in% c(1979, 1991, 1996, 2002), NA, Cohort))
#Black rockfish
black <- plotages %>%
  filter(SPECIES == 'Black')

scale_b <- max(black$n) / max(black$AGE)


brf_age <- ggplot(data = black) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_b), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 65, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_b, name = 'Sample Size', breaks = seq(0, 1500, 250))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Black Rockfish Age Comps',
       subtitle = 'SFmgmtarea=PWS') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

brf_age


#Yelloweye rockfish
Yelloweye <- plotages %>%
  filter(SPECIES == 'Yelloweye')

scale_y <- max(Yelloweye$n) / max(Yelloweye$AGE)

ye_age <- ggplot(data = Yelloweye) +
  geom_point(aes(x = YEAR, y = AGE, size = pij), shape = 21, color = 'black', fill = "grey", stroke = 0.1, alpha = 0.7) +
  geom_line(aes(x = YEAR, y = n / scale_y), color = 'grey', size = 1.4, linetype = 'solid', alpha = 0.4) +
  scale_x_continuous(breaks = seq(1995, 2023, 3), labels = as.character(seq(1995, 2023, 3)), name = 'Year') +
  scale_y_continuous(breaks = seq(0, 110, 5), name = 'Age',
                     sec.axis = sec_axis(~ . * scale_y, name = 'Sample Size', breaks = seq(0, 800, 200))) +  # Add a second Y-axis for 'n'
  scale_size_continuous(guide = FALSE) +
  scale_fill_continuous(guide = FALSE) +
  theme_bw() +
  labs(title = 'Yelloweye Rockfish Age Comps',
       subtitle = 'SFmgmtarea=PWS') +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold and centered title
    plot.subtitle = element_text(face = "bold", hjust = 0.5),    # Bold and centered subtitle
    axis.title.x = element_text(face = "bold", hjust = 0.5),     # Bold and centered x-axis title
    axis.title.y = element_text(face = "bold", hjust = 0.5),     # Bold and centered y-axis title
    axis.title.y.right = element_text(face = "bold", hjust = 0.5) # Bold and centered secondary y-axis title
  )

ye_age
