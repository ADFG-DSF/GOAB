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
    STATAREA = case_when(
      YEAR >= 2021 ~ case_when(
      !is.na(ADFGSTATLING) ~ ADFGSTATLING,
      is.na(ADFGSTATLING) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      is.na(ADFGSTATLING) & !is.na(ADFGSTATOTH) ~ ADFGSTATOTH
      ),
      TRUE ~ ADFGSTAT
    )
  ) %>%
  area_split_sf() %>% 
  filter(!is.na(STATAREA), # Remove rows with missing stat area or salmon stat area
               STATAREA > 100000)
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
    select(PORT, YEAR, STATAREA, SFmgmtarea)
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
  mutate(n = CI + KOD + NG + PWS,
         pCI = CI / n,
         pKod = KOD / n,
         pNG = NG / n,
         pPWS = PWS / n,
         SEpCI = sqrt(pCI * (1 - pCI) / (n - 1)),
         SEpKod = sqrt(pKod * (1 - pKod) / (n - 1)),
         SEpNG = sqrt(pNG * (1 - pNG) / (n - 1)),
         SEpPWS = sqrt(pPWS * (1 - pPWS) / (n - 1))) %>% 
  select(-c("CI", "KOD", "NG", "PWS"))

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
         KOD = if_else(is.na(KOD), 0, KOD),
         NG = if_else(is.na(NG), 0, NG),
         PWS = if_else(is.na(PWS), 0, PWS)) %>%
  mutate(n = CI + KOD + NG + PWS,
         pCI = CI/n,
         pKod = KOD/n,
         pNG = NG/n,
         pPWS = PWS/n,
         SEpCI = sqrt(pCI*(1-pCI)/(n-1)),
         SEpKod = sqrt(pKod*(1-pKod)/(n-1)),
         SEpNG = sqrt(pNG*(1-pNG)/(n-1)),
         SEpPWS = sqrt(pPWS*(1-pPWS)/(n-1))) 

gt(
byAreaRelT %>% 
  filter(PORT == "Seward") %>% 
  select(PORT, USER, YEAR, CI, KOD, NG, PWS, pCI, SEpCI, pKod, SEpKod, pNG, SEpNG, pPWS, SEpPWS) %>%
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
get_data("data/LC/")

library(plyr)

lc <- do.call(rbind.fill, list(ling9104, ling2005,
                                ling2006, ling2007, ling2008, ling2009, ling2010,
                                ling2011, ling2012, ling2013, ling2014,ling2015, 
                                ling2016, ling2017, ling2018, ling2019, ling2020, 
                                ling2021))
detach(package:plyr)

lc$USER[lc$USER %in% c("HomCPort", "HomCSea" )] <- "Charter"


awl <- lc %>%
  filter(USER != 'SewMilC', USER != 'Unknown', USER != 'Elfin_Co', USER != 'Gustavus', USER != "Juneau") %>%
  filter(!(PORT %in% c('SandPt', 'Cordova'))) %>%
  filter(!is.na(STATAREA)) %>%
  mutate(
    USER = case_when(
      USER == 'P' ~ 'Private',
      USER == 'C' ~ 'Charter',
      TRUE ~ USER
    )
    ) %>% 
  area_split_sf()
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
  filter(YEAR == 2004, SFmgmtarea == "ZZZ") %>%
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
  mutate(n = CI + KOD + NG + PWS,
         pCI = CI / n,
         pKod = KOD / n,
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
  geom_point(aes(y = pKod, color = "KOD")) +
  geom_point(aes(y = pNG, color = "NG")) +
  geom_point(aes(y = pPWS, color = "PWS")) +
  scale_color_manual(name = "Mgmt Area", values = c(CI = "red", KOD = "green", NG = "blue", PWS = "purple")) +
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


