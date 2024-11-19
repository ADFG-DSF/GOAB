###############################################################################
# AWLsummaries.SAS - misc summaries of rockfish data.
# 
# Updated for use in 1996-2015 RF report BF 10/02/16
# Used by MDS 2/2/21 updating RF report through 2019
#
# Translated from SAS to R by CWM on 06/08/23
###############################################################################

library(tidyverse)
library(gt)

source("functions.R")

#Function call
get_data("data/RF/")

get_data("data/Intervw/")



#Combine datasets
rock2020$LENGTH <- rock2020$FORK_LENGTH
rf <- bind_rows(rock2019, rock2020, rock2021) 

#Filter rows for year greater than 2019, data check
rf <- rf %>% filter(YEAR>=2019)



# Calculate summary statistics for age and length by species
summary_stats <- rf %>%
  filter(!is.na(SPECIES)) %>% 
  group_by(SPECIES) %>%
  summarise(minage = min(AGE, na.rm = TRUE),
            maxage = max(AGE, na.rm = TRUE),
            nage = sum(!is.na(AGE)),
            minlen = min(LENGTH, na.rm = TRUE),
            maxlen = max(LENGTH, na.rm = TRUE),
            nlen = sum(!is.na(LENGTH))) %>% 
  mutate(
    minage = case_when(
      minage > 1000 ~ NA_real_,
      TRUE ~ minage
    ),
    maxage = case_when(
      maxage < 0 ~ NA_real_,
      TRUE ~ maxage
    )
  )




# Calculate the mean age by species and sex
mean_age <- rf %>%
  group_by(SPECIES, SEX) %>%
  summarise(meanage = mean(AGE, na.rm = TRUE), .groups = "drop")

# Print the 'mean_age' dataset
gt(mean_age)

#number of rf sampled by species, year and port for biological characteristics: 
#this should match the first table from SampleSize_bySpecies.sas

# Perform frequency analysis
freq_table <- rf %>%
  group_by(PORT, YEAR, USER) %>%
  summarise(count = n()) %>%
  ungroup()

# Print the frequency table with a title

gt(freq_table) %>% 
  tab_header("work.biosamp - number of rf sampled for bio characteristics by port sp and year")

#assemblage composition of harvest sample from awl data
# Create a new dataset 'rfawl' by copying 'rf'
rfawl <- rf

# Create a new variable 'assemb' based on the condition
rfawl$assemb <- ifelse(rfawl$SP %in% c(142, 154, 172, 173, 155, 156, 169), 'pel', 'npel')


# Perform frequency analysis
freq_table <- rfawl %>%
  group_by(PORT, USER, YEAR, assemb) %>%
  summarise(count = n()) %>%
  ungroup()

# Print the frequency table with a title

gt(freq_table) %>% 
  tab_header("Number sampled by assemblage, port, user, and year (raw awl data)")
