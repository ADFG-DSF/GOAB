################################################################################
# Fraction cleaned at sea
#
# Translated from SAS to R by CWM 06/08/23
################################################################################


library(tidyverse)
library(gt)


source("functions.R")

#Function call
get_data("data/Intervw/")

#Make dataframe

int21$YEAR <- 2021 

int22$YEAR <- 2022

library(plyr)
int <- do.call(rbind.fill, list(int05, int06, int07, int08,
                                int09, int10, int11, int12, int13, int14,
                                int15, int16, int17, int18, int19, int20,
                                int21, int22))
detach(package:plyr) 
# Sort the 'int' dataframe by port, user, and year
int <- int %>% arrange(PORT, USER, YEAR)
# Calculate the sum of HaCAS and HaKept by port, user, and year
summary_stats <- int %>%
  group_by(PORT, USER, YEAR) %>%
  summarise(HACAS = sum(HACAS, na.rm = TRUE),
            HAKEPT = sum(HAKEPT, na.rm = TRUE),
            .groups = "drop")

# Create a new dataframe 'PercentCAS' and filter rows
PercentCAS <- summary_stats %>%
  filter(PORT == "Homer", YEAR >= 2017) %>%
  mutate(PCAS = HACAS / HAKEPT * 100)

# Print the 'PercentCAS' dataframe with a formatted column
cat("Percent of halibut cleaned at sea\n\n")
PercentCAS$PCAS <- format(PercentCAS$PCAS, digits = 3, nsmall = 0)
PercentCAS

#Mean proportion of halibut cleaned at sea for vessel trips with halibut cleaned at sea only
# Filter rows where HACAS is not equal to 0
int_CAS <- int %>% filter(HACAS > 0) %>% 
  arrange(PORT, USER, YEAR)

# Calculate the mean of pCAS by port, user, and year
summary_stats <- int_CAS %>%
  group_by(PORT, USER, YEAR) %>%
  summarise(Mean_pCAS = mean(PCAS, na.rm = TRUE), .groups = "drop")

# Print the summary statistics dataframe with a title

gt(summary_stats) %>% 
  tab_header("For trips with halibut cleaned at sea, the mean proportion cleaned at sea")



# Create boxplots for pCAS by port and year
boxplot <- int %>%
  ggplot(aes(x = YEAR, y = PCAS, fill = factor(PORT))) +
  geom_boxplot() +
  #scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Print the boxplot
gt(boxplot)

#Percent rockfish cleaned at sea by port and user

# Calculate the sum of TRFCAS and TRFKEPT by port, user, and year
summary_stats <- int %>%
  group_by(PORT, USER, YEAR) %>%
  summarise(TRFCAS = sum(TRFCAS, na.rm = TRUE),
            TRFKEPT = sum(TRFKEPT, na.rm = TRUE),
            .groups = "drop")

# Create a new dataframe 'RFPercentCAS' and calculate PCAS
RFPercentCAS <- summary_stats %>%
  mutate(PCAS = TRFCAS / TRFKEPT * 100)

# Print the 'RFPercentCAS' dataframe with a formatted column

RFPercentCAS$PCAS <- format(RFPercentCAS$PCAS, digits = 3, nsmall = 0)
gt(RFPercentCAS) %>% 
  tab_header("Percent of rockfish cleaned at sea by port and user")

#Percent Lingcod cleaned at sea by port and user
# Sort the 'int' dataframe by port, user, and year
int <- int %>% arrange(PORT, USER, YEAR)

# Calculate the sum of LCCAS and LCKept by port, user, and year
summary_stats <- int %>%
  group_by(PORT, USER, YEAR) %>%
  summarise(LCCAS = sum(LCCAS, na.rm = TRUE),
            LCKEPT = sum(LCKEPT, na.rm = TRUE),
            .groups = "drop")

# Create a new dataframe 'LCPercentCAS' and calculate PCAS
LCPercentCAS <- summary_stats %>%
  mutate(PCAS = LCCAS / LCKEPT * 100)

# Print the 'LCPercentCAS' dataframe with a formatted column

LCPercentCAS$PCAS <- format(LCPercentCAS$PCAS, digits = 3, nsmall = 0)
gt(LCPercentCAS) %>% 
  tab_header("Percent of lingcod cleaned at sea by port and user")
