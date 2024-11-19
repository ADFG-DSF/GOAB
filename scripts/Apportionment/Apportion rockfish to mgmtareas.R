################################################################################
# Apportion rockfish to SFmgmtareas.sas
# 
# This code estimates the proportions of rockfish harvest and release by management
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
# MDS 8-25-20 modified to divide PWS into EPWS and WPWS for PWS AMR
#
# Translated from SAS to R by CWM 05/31/23
################################################################################

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


int <- int_all %>% 
  mutate(
    # Calculate total Rockfish caught
    pelcatch = PELKEPT + PELREL,
    yecatch = YEKEPT + YEREL,
    npcatch = NPKEPT + NPREL,
  #  RFKEPT = PELKEPT + YEKEPT + NPKEPT,
    RFKEPT = case_when(
      YEAR > 2010  ~ PELKEPT + YEKEPT + NPKEPT,  # Yelloweye were recorded seperately from Non-pelagic starting 2011
      YEAR <= 2010 & YEAR > 2000 ~ PELKEPT + NPKEPT, # Rockfish recorded by assemblage only starting in 2001
      YEAR <= 2000 ~ RFKEPT + PELKEPT + NPKEPT # Prior to 2001, rockfish were recorded without assemblage seperately in some instances
    ),
    # RFREL = case_when(
    #   is.na(RFREL) | RFKEPT == 0 ~ PELREL + YEREL + NPREL,
    #   TRUE ~ RFREL
    # ),
  RFREL = case_when(
    YEAR > 2010  ~ PELREL + YEREL + NPREL,  # Yelloweye were recorded seperately from Non-pelagic starting 2011
    YEAR <= 2010 & YEAR > 2000 ~ PELREL + NPREL, # Rockfish recorded by assemblage only starting in 2001
    YEAR <= 2000 ~ RFREL + PELREL + NPREL # Prior to 2001, rockfish were recorded without assemblage seperately in some instances
  ),
    rfcatch = RFKEPT + RFREL,
    # Adjust for changes to stat area protocol in 2021
  ADFGSTAT = case_when(
    YEAR >= 2021 ~ case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATOTH) ~ ADFGSTATOTH
    ),
    TRUE ~ ADFGSTAT
  )
  ) %>%
  area_split_sf() %>% 
  filter(PORT != "SandPt" & PORT != "Cordova",
               # Remove rows where 'user' is equal to 'Military'
               USER != "Military",
               # Remove rows where ADFGSTAT is not available or less than 100000 (i.e. salmon stat areas reported in 1994)
               ADFGSTAT  > 0, ADFGSTAT >= 100000)
##Work with the data
#SFmgmtarea assignments - all years pooled
gt(
  int %>%
    count(SFmgmtarea, PORT, name = "count") %>%
    group_by(SFmgmtarea) %>%
    mutate(percent = count / sum(count) * 100) %>%
    ungroup()
)

############################################################################
#Estimate SFmgmtarea of capture proportions for Rockfish harvest by port using interview data
############################################################################

# intnoNA <- int %>% # Remove interviews with no kept rockfish
#   filter(RFKEPT > 0)

gt(
int %>%
  arrange(PORT, USER, YEAR) %>% # sort by port, user, and year
  group_by(PORT, USER, YEAR) %>% # group by port, user, and year
  reframe(SFmgmtarea = unique(SFmgmtarea), w_rfkept = sum(RFKEPT, na.rm = TRUE)) %>% # calculate sum of RFKEPT for each unique combination of port, user, year, and SFmgmtarea
  # ungroup() %>% # remove grouping
  group_by(PORT, SFmgmtarea) %>% # group by port and SFmgmtarea
  reframe(prop = sum(w_rfkept) / sum(int$RFKEPT, na.rm = TRUE)) # calculate the proportion of RFKEPT for each SFmgmtarea and port, using the total sum of RFKEPT in the dataset
) %>% 
  tab_header(
    title = 'Proportion of Rockfish Catch by Port and Management area of capture',
    subtitle = 'All years combined'
  )

# Estimate SFmgmtarea of capture proportions for Rockfish harvest by port using interview data
bySFmgmtarea <- int %>%
  group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = sum(RFKEPT, na.rm = TRUE)) %>%
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

# Print the data for Seward
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
  ggtitle('Percent of PWS Rockfish harvest by mgmt area (interview data)')


# Create a frequency table by port, user, year, and SFmgmtarea, weighted by RFREL
bySFmgmtareaRel <- int %>% group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = sum(RFREL)) %>%
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
  select(PORT, USER, YEAR, CI, Kod, NG, PWS, pCI, SEpCI, pKod, SEpKod, pNG, SEpNG, pPWS, SEpPWS)
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
  ggtitle("Percent of Rockfish releases by mgmt area (interview data)")

#Harvest proportion analysis as above but using biological samples
#load in and combine data
get_data("O:/DSF/GOAB/R Data/RF/")

library(plyr)
awl <- data.frame()
awl <- do.call(rbind.fill, list(rock9195, rock9600,
                                rock2001, rock2002, rock2003, rock2004, rock2005,
                                rock2006, rock2007, rock2008, rock2009, rock2010,
                                rock2011, rock2012, rock2013, rock2014,rock2015, 
                                rock2016, rock2017, rock2018, rock2019, rock2020, 
                                rock2021, rock2022, rock2023))
detach(package:plyr)

awl$USER[awl$USER %in% c("HomCPort", "HomCSea" )] <- "Charter"


awl <- awl %>%
  filter(USER != 'SewMilC', USER != 'Unknown', USER != 'Elfin_Co', USER != 'Gustavus', USER != "Juneau") %>%
  filter(!(PORT %in% c('SandPt', 'Cordova'))) %>%
  filter(!is.na(STATAREA)) %>% 
  area_split_sf()
#check SFmgmtarea assignments
awl %>%
  filter(
    !is.na(SFmgmtarea),
    SFmgmtarea != 'ZZZ'
    ) %>%
  group_by(PORT, YEAR, SFmgmtarea) %>%
  summarize(n = n()) %>%
  group_by(PORT, YEAR) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from = SFmgmtarea, values_from = c(n, percent), 
              #names_prefix = "count_", values_prefix = "n_",
              values_fill = list(n_percent = 0)) %>%
  select(PORT, YEAR, contains("count"), contains("percent"), -ends_with("_percent"))


awl %>%
  filter(YEAR == 2004, SFmgmtarea == "Err") %>%
  select(USER, PORT, YEAR, SFmgmtarea, STATAREA)
#Estimate proportions of Seward Rockfish harvest by SFmgmtarea
# sort the data
awl_sorted <- awl %>%
  arrange(PORT, USER, YEAR)

# get frequency table
freq_table <- awl_sorted %>%
  group_by(PORT, USER, YEAR, SFmgmtarea) %>%
  summarise(count = n()) %>%
  ungroup() 

gt(freq_table) %>% 
  tab_header(
    title = 'Number of rockfish by Port, User, Mgmt Area by Year'
  )

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

gt(transposed_data) %>% 
  tab_header(
    title = 'Count and proportion of rockfish by Port, User, Year, and Mgmt Area'
  )


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
  labs(title = "Percent of Rockfish releases by mgmt area (interview data)",
       x = "Year", y = "Percent",
       color = "Mgmt Area") 

#Visually compare harvest apportioning between interview and bio data
##Compare Seward harvest proportions
Sew_int <- byAreaT %>% 
  filter(PORT == 'Seward') %>% 
  select(PORT, USER, YEAR, 
         #NGint = NG, PWSint = PWS, 
         pPWSint = pPWS,
         SEpPWSint = SEpPWS)

Sew_bio <- byAreaTbio %>% 
  filter(PORT == 'Seward') %>% 
  select(PORT, USER, YEAR, 
         #NGbio = NG, PWSbio = PWS, 
         pPWSbio = pPWS,
         SEpPWSbio = SEpPWS)

Seward_compare <- full_join(Sew_int, Sew_bio, by = c("PORT", "USER", "YEAR")) 

#write.table(Seward_compare, file = "Seward_compare.csv", sep = ",", row.names = FALSE)

Seward_plot <- ggplot(Seward_compare, aes(x = YEAR)) +
  facet_wrap(~ USER, ncol = 1, scales = "free_y") +
  geom_line(aes(y = pPWSint), colour = "blue") +
  geom_line(aes(y = pPWSbio), colour = "red") +
  ylim(0, 1) +
  ggtitle("Seward - percent Rockfish harvest from PWS, int vs bio data")

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
  ggtitle("Homer - percent Rockfish harvest from PWS, int vs bio data")

print(Homer_plot)


