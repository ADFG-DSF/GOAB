################################################################################
# Secondary Objective 5 - 
# Estimate the proportions of released lingcod that were of sublegal (under 35 
# inches total length) and legal size (35 inches and greater) for ports with a 
# minimum size limit regulation. These data will provide information on future 
# recruitment and abundance indices used for future stock assessments.
#
# Made by CWM on 02/06/25
################################################################################
library(tidyverse)
library(gt)


source("functions.R")

#Function call
get_data("data/LC/")

#####

library(plyr)

lc <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, 
                               ling2009, ling2010, ling2011, ling2012, ling2013, 
                               ling2014, ling2015, ling2016, ling2017, ling2018, 
                               ling2019, ling2020, ling2021, ling2022, ling2023,
                               ling2024))


detach(package:plyr)

under35 <- lc %>% 
  filter(
    !is.na(LENGTH), # Remove entries with no length, can't judge legality
    YEAR >= 2003 # When size restrictions went into place
  ) %>% 
  area_split_sf() %>% 
  filter(
    !(SFmgmtarea %in% c('KOD'))
  ) %>% 
  mutate(
    # Sort lingcod by size into legal and sublegal categories
    # min size limit of 88.9 cm (35 in)
    SUBLENGTH = case_when(
      LENGTH < 88.9 ~ 1,
      LENGTH >= 88.9 ~ 0
    ),
    NLENGTH = 1
  )

sublegal <- under35 %>% 
  filter(USER != 'Unknown') %>% 
  group_by(YEAR, PORT, USER) %>% 
  # sum sublength is total sublegal lingcod sampled. 
  # sum nlength is total lingcod sampled. 
  # nlength will be useful in calculating the stderr of the proportion nsub/n.
  reframe(
    n = sum(NLENGTH),
    n_sublegal = sum(SUBLENGTH),
    pSublegal = n_sublegal/n,
    SESublegal = sqrt(pSublegal*(1-pSublegal)/(n-1))
  )

gt(sublegal) %>% 
  tab_header(title = "Proportion of sublegal lingcod (under 35 in.) by Year, Port, and User")
# Pooled users
sublegal2 <- under35 %>% 
  group_by(YEAR, PORT) %>% 
  # sum sublength is total sublegal lingcod sampled. 
  # sum nlength is total lingcod sampled. 
  # nlength will be useful in calculating the stderr of the proportion nsub/n.
  reframe(
    n = sum(NLENGTH),
    n_sublegal = sum(SUBLENGTH),
    pSublegal = n_sublegal/n,
    SESublegal = sqrt(pSublegal*(1-pSublegal)/(n-1))
  )

gt(sublegal2) %>% 
  tab_header(
    title = "Proportion of sublegal lingcod (under 35 in.) by Year and Port",
    subtitle = "Pooled User groups"
    )


