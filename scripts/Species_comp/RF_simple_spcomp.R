################################################################################
# RFSummary.sas A summary of sport harvested rockfish
# bjf 10/10/14
# MDS 10/23/18 for project review
# 
# Translated by from SAS to R by CWM on 05/30/23
################################################################################

library(tidyverse)

source("functions.R")

#Function call
get_data("data/RF/")


#Combine data
library(plyr)
rock <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                     rock2004, rock2005, rock2006, rock2007, rock2008, 
                                     rock2009, rock2010, rock2011, rock2012, rock2013, 
                                     rock2014, rock2015, rock2016, rock2017, rock2018, 
                                     rock2019, rock2020, rock2021, rock2022))
detach(package:plyr)

#
rock <- rock %>%
  filter(ASSEMB != '') %>%
  mutate(USER = if_else(USER == '', 'Unknown', USER)) %>%
  filter(STATAREA != '') %>%
  filter(STATAREA >= 100000)  %>%
  area_split_sf() %>% 
  mutate(SFmgmtarea = if_else(SFmgmtarea == 'PWS' & STATAREA / 10000 < 47, 'EPWS', SFmgmtarea)) %>%
  mutate(SFmgmtarea = if_else(SFmgmtarea == 'PWS' & STATAREA / 10000 > 47, 'WPWS', SFmgmtarea))



# Sort the 'rock' dataset by SFmgmtarea, year, and species
rock <- rock %>% arrange(SFmgmtarea, YEAR, SPECIES)

# Perform frequency analysis
spcomp <- rock %>% 
  group_by(SFmgmtarea, YEAR, SPECIES) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Print the resulting dataset
spcomp                


# Export the filtered dataset to a CSV file
write_csv(spcomp, "O:/DSF/GOAB/Data_requests/Mike_Booz/AMR/spcomp9122LCI.csv")
