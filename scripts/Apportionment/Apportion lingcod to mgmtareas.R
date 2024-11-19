###############################################################################
# Apportion lingcod to SFmgmtareas.sas
# 
# This code estimates the proportions of lingcod harvest and release by management
# area. In other words, it apportions reported harvest and release from port
# sampling interviews to the SFmgmtarea of capture. 
# 
# Use these data to reapportion harvest estimates to management areas
# 
# Estimates of harvest and release are based on interview data. Alternate set
# of proportions were calculated for harvested fish based on AWL sample sizes.
# 
# SCM 9-17-14
# SCM 9-14-17 updated thru 2016
# - check to see if calculates SE of NG/PWS proportions???
#   SRW 7-5-19 updated thru 2017
#
# Translated from SAS to R by CWM 05/30/23
###############################################################################

library(tidyverse)
library(data.table)
library(gt)

source("functions.R")

#Function call
get_data("data/Intervw/")

#Make dataframe

int21$YEAR <- 2021 

int22$YEAR <- 2022

library(plyr)

int_all <- do.call(rbind.fill, list(int9204, int05, int06, int07, int08,
                                int09, int10, int11, int12, int13, int14,
                                int15, int16, int17, int18, int19, int20,
                                int21, int22, int23))
detach(package:plyr)


int <- int_all %>% filter(PORT != "SandPt",  PORT != "Cordova", # Remove rows with ports of 'SandPt' or 'Cordova'
                          # Remove rows where 'user' is 'Military'
                          USER != "Military") %>%  
  mutate(
    # Calculate total lingcod caught
    lccatch = LCKEPT + LCREL,
    # Adjust for changes to stat area protocol in 2021
    ADFGSTAT = case_when(
      YEAR >= 2021 ~ case_when(
      !is.na(ADFGSTATLING) ~ ADFGSTATLING,
      is.na(ADFGSTATLING) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      is.na(ADFGSTATLING) & !is.na(ADFGSTATOTH) ~ ADFGSTATOTH
      ),
      TRUE ~ ADFGSTAT
    )
  ) %>%
  area_split_sf() %>% 
  filter(!is.na(ADFGSTAT), # Remove rows with missing stat area or salmon stat area
               ADFGSTAT > 100000)
##Work with the data
#SFmgmtarea assignments - all years pooled
gt(
  int %>%
    count(SFmgmtarea, PORT, name = "count") %>%
    group_by(SFmgmtarea) %>%
    mutate(percent = count / sum(count) * 100) %>%
    ungroup()
)

## Print Errors
gt(
  int %>% 
    filter(SFmgmtarea == 'ZZZ') %>% 
    select(PORT, YEAR, ADFGSTAT, SFmgmtarea)
)

#Estimate SFmgmtarea of capture proportions for lingcod harvest by port using interview data
intnoNA <- int %>%
  filter(LCKEPT > 0,
         SFmgmtarea != 'ZZZ')

gt(
intnoNA %>%
  arrange(PORT, USER, YEAR) %>% # sort by port, user, and year
  group_by(PORT, USER, YEAR) %>% # group by port, user, and year
  reframe(SFmgmtarea = unique(SFmgmtarea), w_LCKEPT = sum(LCKEPT)) %>% # calculate sum of LCKEPT for each unique combination of port, user, year, and SFmgmtarea
  # ungroup() %>% # remove grouping
  group_by(PORT, SFmgmtarea) %>% # group by port and SFmgmtarea
  reframe(prop = sum(w_LCKEPT) / sum(intnoNA$LCKEPT)) # calculate the proportion of LCKEPT for each SFmgmtarea and port, using the total sum of LCKEPT in the dataset
)

# Estimate SFmgmtarea of capture proportions for lingcod harvest by port using interview data
bySFmgmtarea <- intnoNA %>%
  group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = sum(LCKEPT)) %>%
  ungroup()

# Transpose the data
byAreaT <- bySFmgmtarea %>%
  # select(-count) %>%
  spread(SFmgmtarea, count)

# Reshape the data from wide to long format
byAreaT <- bySFmgmtarea %>% 
  pivot_wider(names_from = SFmgmtarea, values_from = count, values_fill = 0) %>% 
  mutate(n = CI + Kod + NG + PWS,
         pCI = CI / n,
         pKod = Kod / n,
         pNG = NG / n,
         pPWS = PWS / n,
         SEpCI = sqrt(pCI * (1 - pCI) / (n - 1)),
         SEpKod = sqrt(pKod * (1 - pKod) / (n - 1)),
         SEpNG = sqrt(pNG * (1 - pNG) / (n - 1)),
         SEpPWS = sqrt(pPWS * (1 - pPWS) / (n - 1))) %>% 
  select(-c("CI", "Kod", "NG", "PWS"))

# Print the data for port='Seward'
gt(
byAreaT %>% 
  filter(PORT == 'Seward') %>% 
  select(PORT, USER, YEAR, starts_with("p"), starts_with("SEp"))
)

# Plot pPWS for harvest
ggplot(byAreaT, aes(x = YEAR, y = pPWS)) +
  geom_point() +
  facet_wrap(~PORT+USER, ncol = 2, scales = 'free_y') +
  ylim(0, 1) +
  ggtitle('Percent of PWS lingcod harvest by mgmt area (interview data)')

# Sort the data by port, user, and year
int <- int %>% arrange(PORT, USER, YEAR)

# Create a frequency table by port, user, year, and SFmgmtarea, weighted by harel
bySFmgmtareaRel <- int %>% group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = sum(LCREL)) %>%
  ungroup()

# Reshape the data using the transpose function from the tidyr package
byAreaRelT <- bySFmgmtareaRel %>% pivot_wider(names_from = SFmgmtarea, values_from = count, values_fill = 0)
#Calculate SFmgmtarea Porportions
byAreaRelT <- byAreaRelT %>%
  mutate(CI = if_else(is.na(CI), 0, CI),
         Kod = if_else(is.na(Kod), 0, Kod),
         NG = if_else(is.na(NG), 0, NG),
         PWS = if_else(is.na(PWS), 0, PWS)) %>%
  mutate(n = CI + Kod + NG + PWS,
         pCI = CI/n,
         pKod = Kod/n,
         pNG = NG/n,
         pPWS = PWS/n,
         SEpCI = sqrt(pCI*(1-pCI)/(n-1)),
         SEpKod = sqrt(pKod*(1-pKod)/(n-1)),
         SEpNG = sqrt(pNG*(1-pNG)/(n-1)),
         SEpPWS = sqrt(pPWS*(1-pPWS)/(n-1))) 

gt(
byAreaRelT %>% 
  filter(PORT == "Seward") %>% 
  select(PORT, USER, YEAR, CI, Kod, NG, PWS, pCI, SEpCI, pKod, SEpKod, pNG, SEpNG, pPWS, SEpPWS) %>%
  print(n = Inf)
)
#plot pPWS for releases
byAreaRelT_s <- byAreaRelT %>% filter(USER %in% c("Charter", "Private"))

ggplot(data = byAreaRelT_s, aes(x = YEAR)) +
  geom_point(aes(y = pCI), color = "blue") +
  geom_point(aes(y = pKod), color = "red") +
  geom_point(aes(y = pNG), color = "green") +
  geom_point(aes(y = pPWS), color = "orange") +
  facet_grid(PORT ~ USER) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Percent of lingcod releases by mgmt area (interview data)")

#Harvest proportion analysis as above but using biological samples
#load in and combine data
get_data("O:/DSF/GOAB/R Data/LC/")

library(plyr)

awl <- do.call(rbind.fill, list(ling9104, ling2005,
                                ling2006, ling2007, ling2008, ling2009, ling2010,
                                ling2011, ling2012, ling2013, ling2014,ling2015, 
                                ling2016, ling2017, ling2018, ling2019, ling2020, 
                                ling2021))
detach(package:plyr)

awl$USER[awl$USER %in% c("HomCPort", "HomCSea" )] <- "Charter"


awl <- awl %>%
  filter(USER != 'SewMilC', USER != 'Unknown', USER != 'Elfin_Co', USER != 'Gustavus', USER != "Juneau") %>%
  filter(!(PORT %in% c('SandPt', 'Cordova'))) %>%
  filter(!is.na(STATAREA)) %>%
  mutate(
    USER = case_when(
      USER == 'P' ~ 'Private',
      USER == 'C' ~ 'Charter',
      TRUE ~ USER
    ),
    SFmgmtarea = case_when(
    STATAREA <490000 & STATAREA >= 440000 & !(STATAREA %in% c(485832, 485902, 485933, 485934)) ~ 'PWS',
    #int(STATAREA / 10000) < 49 & int(STATAREA / 10000) >= 44 & !(STATAREA %in% c(485832, 485902, 485933, 485934)) ~ 'PWS',
    STATAREA %in% c(485832, 485902, 485933, 485934, 485935, 486002, 495831, 495901, 495902, 495931, 495932, 495933, 495934, 
                    495935, 495936, 495937, 495938, 495939, 496001, 496002, 505831, 505901, 505902, 505903, 505904, 505905, 
                    505906, 505907, 505908, 505909, 505931, 505932, 505933, 505934) ~ 'NG',
    STATAREA %in% c(495800, 495832, 505700, 505730, 505800, 505832, 515630, 515700, 515730, 515801, 515802, 515833, 525600, 
                    525630, 525701, 525702, 525703, 525731, 525732, 525733, 525801, 525802, 525803, 525804, 525805, 525806, 
                    525807, 525832, 525833, 525834, 535601, 535602, 535631, 535632, 535633, 535634, 535701, 535702, 535703, 
                    535704, 535705, 535706, 535707, 535731, 535732, 535733, 535734, 535802, 535803, 535831, 545601, 545602, 
                    545631, 545632, 545633, 545701, 545702, 545703, 545704, 545732, 545733, 545734, 545804, 555630, 555701, 
                    555733) ~ 'Kod',
    STATAREA %in% c(555731, 555732, 545731, 545801, 545802, 545803, 535801, 535832) ~ 'AP',
    STATAREA %in% c(515831,515832,515901,515902,515903,515904,515905,515906,515907,
                    515908,515931,515932,515933,515934,515935,515936,515937,515938,	515939,
                    516001,516002,525831,525835,525836,525837,525901,525902,525931,	525932,
                    526002,526003,535833,535834,535901,535902,535903,535904,535905,535906,
                    535931,535932,535933,545900) ~ 'CI',
    TRUE ~ "Err")) 
#check SFmgmtarea assignments
gt(
awl %>%
  filter(!is.na(SFmgmtarea)) %>%
  group_by(PORT, YEAR, SFmgmtarea) %>%
  summarize(n = n()) %>%
  group_by(PORT, YEAR) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from = SFmgmtarea, values_from = c(n, percent), 
              #names_prefix = "count_", values_prefix = "n_",
              values_fill = list(n_percent = 0)) %>%
  select(PORT, YEAR, contains("count"), contains("percent"), -ends_with("_percent"))
)

awl %>%
  filter(YEAR == 2004, SFmgmtarea == "Err") %>%
  select(USER, PORT, YEAR, SFmgmtarea, STATAREA)
#Estimate proportions of Seward lingcod harvest by SFmgmtarea
# sort the data
awl_sorted <- awl %>%
  arrange(PORT, USER, YEAR)

# get frequency table
freq_table <- awl_sorted %>%
  group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = n()) %>%
  ungroup() 

# transpose the data
transposed_data <- freq_table %>%
  pivot_wider(names_from = SFmgmtarea, values_from = count, values_fill = 0)

# calculate proportions and standard errors
transposed_data <- transposed_data %>%
  mutate(n = CI + Kod + NG + PWS,
         pCI = CI / n,
         pKod = Kod / n,
         pNG = NG / n,
         pPWS = PWS / n,
         SEpCI = sqrt(pCI * (1 - pCI) / (n - 1)),
         SEpKod = sqrt(pKod * (1 - pKod) / (n - 1)),
         SEpNG = sqrt(pNG * (1 - pNG) / (n - 1)),
         SEpPWS = sqrt(pPWS * (1 - pPWS) / (n - 1))) %>%
  select(-n)

# print the final table
gt(transposed_data)

#plot pPWS for bio sample sizes
byAreaTbio <- transposed_data %>% filter(USER %in% c("Charter", "Private"))

ggplot(byAreaTbio, aes(x = YEAR)) +
  geom_point(aes(y = pCI, color = "CI")) +
  geom_point(aes(y = pKod, color = "Kod")) +
  geom_point(aes(y = pNG, color = "NG")) +
  geom_point(aes(y = pPWS, color = "PWS")) +
  scale_color_manual(name = "Mgmt Area", values = c(CI = "red", Kod = "green", NG = "blue", PWS = "purple")) +
  facet_grid(cols = vars(PORT), rows = vars(USER), switch = "both") +
  theme_minimal() +
  labs(title = "Percent of lingcod releases by mgmt area (interview data)",
       x = "Year", y = "Percent",
       color = "Mgmt Area") 

#Visually compare harvest apportioning between interview and bio data
##Compare Seward harvest proportions
Sew_int <- byAreaT %>% 
  filter(PORT == 'Seward') %>% 
  select(PORT, USER, YEAR, 
         #NGint = NG, PWSint = PWS, 
         pPWSint = pPWS)

Sew_bio <- byAreaTbio %>% 
  filter(PORT == 'Seward') %>% 
  select(PORT, USER, YEAR, 
         #NGbio = NG, PWSbio = PWS, 
         pPWSbio = pPWS)

Seward_compare <- full_join(Sew_int, Sew_bio, by = c("PORT", "USER", "YEAR")) 

#write.table(Seward_compare, file = "Seward_compare.csv", sep = ",", row.names = FALSE)

Seward_plot <- ggplot(Seward_compare, aes(x = YEAR)) +
  facet_wrap(~ USER, ncol = 1, scales = "free_y") +
  geom_line(aes(y = pPWSint), colour = "blue") +
  geom_line(aes(y = pPWSbio), colour = "red") +
  ylim(0, 1) +
  ggtitle("Seward - percent lingcod harvest from PWS, int vs bio data")

print(Seward_plot)

##Compare Homer harvest proportions
Homer_int <- byAreaT %>% 
  filter(PORT == 'Homer') %>% 
  select(PORT, USER, YEAR, 
         #NGint = NG, PWSint = PWS, 
         pPWSint = pPWS)

Homer_bio <- byAreaTbio %>% 
  filter(PORT == 'Homer') %>% 
  select(PORT, USER, YEAR, 
         #NGbio = NG, PWSbio = PWS, 
         pPWSbio = pPWS)

Homer_compare <- full_join(Homer_int, Homer_bio, by = c("PORT", "USER", "YEAR")) 

#write.table(Homer_compare, file = "Homer_compare.csv", sep = ",", row.names = FALSE)

Homer_plot <- ggplot(Homer_compare, aes(x = YEAR)) +
  facet_wrap(~ USER, ncol = 1, scales = "free_y") +
  geom_line(aes(y = pPWSint), colour = "blue") +
  geom_line(aes(y = pPWSbio), colour = "red") +
  ylim(0, 1) +
  ggtitle("Homer - percent lingcod harvest from PWS, int vs bio data")

print(Homer_plot)


