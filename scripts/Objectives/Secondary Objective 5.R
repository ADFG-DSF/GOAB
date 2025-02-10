################################################################################
# Secondary Objective 5 - 
# Estimate the proportions of released lingcod that were of sublegal (under 35 
# inches total length) and legal size (35 inches and greater) for ports with a 
# minimum size limit regulation. These data will provide information on future 
# recruitment and abundance indices used for future stock assessments.
#
# Made by CWM on 02/10/25
################################################################################
library(tidyverse)
library(gt)
source("functions.R")


get_data("data/Intervw/")


###

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021

int22$YEAR <- 2022

library(plyr)
int <- do.call(rbind.fill, list(intervw9219, int9204, int05, int06, int07, 
                                int08, int09, int10, int11, int12, int13, 
                                int14, int15, int16, int16prelim, int17, 
                                int18, int19, int20, int21, int22, int23,
                                int24))
detach(package:plyr)


# LCRELOVR - lingcod released over limit
# LCRELUND - lingcod released under limit


ling_rel <- int %>% 
  filter(
    !(PORT %in% c('CCI', 'SandPt')), # remove ports with small sample size and no longer recorded
    YEAR >= 2007, # Year when over/under size lingcod release started being recorded
    (!is.na(LCRELOVR) | !is.na(LCRELUND)) # only looking at trips with released lingcod
    ) %>% 
  mutate(
    STATAREA = case_when(
      !is.na(ADFGSTATLING) ~ ADFGSTATLING, # If the interview has a halibut specific stat area this is used
      is.na(ADFGSTATLING) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI, # if the interview has a combined area (multi target) and no halibut, this is used
      is.na(ADFGSTATLING) & is.na(ADFGSTATCOMBI)  & !is.na(ADFGSTAT) ~ ADFGSTAT
    )
  ) %>% 
  area_split_sf() %>% 
  filter(
    SFmgmtarea != 'KOD' # No size restrictions in Kodiak
  )


ling_rel_port_user <- ling_rel %>% 
  group_by(YEAR, PORT, USER) %>% 
  # sum sublength is total sublegal lingcod sampled. 
  # sum nlength is total lingcod sampled. 
  # nlength will be useful in calculating the stderr of the proportion nsub/n.
  reframe(
    n_legal = sum(LCRELOVR, na.rm = TRUE),
    n_sublegal = sum(LCRELUND, na.rm = TRUE),
    n = n_legal + n_sublegal,
    pSublegal = n_sublegal/n,
    SESublegal = sqrt(pSublegal*(1-pSublegal)/(n-1))
  )


gt(ling_rel_port_user) %>% 
  tab_header(title = "Proportion of sublegal lingcod (under 35 in.) released by Year, Port, and User")


ling_rel_port <- ling_rel %>% 
  group_by(YEAR, PORT) %>% 
  # sum sublength is total sublegal lingcod sampled. 
  # sum nlength is total lingcod sampled. 
  # nlength will be useful in calculating the stderr of the proportion nsub/n.
  reframe(
    n_legal = sum(LCRELOVR, na.rm = TRUE),
    n_sublegal = sum(LCRELUND, na.rm = TRUE),
    n = n_legal + n_sublegal,
    pSublegal = n_sublegal/n,
    SESublegal = sqrt(pSublegal*(1-pSublegal)/(n-1))
  )


gt(ling_rel_port) %>% 
  tab_header(
    title = "Proportion of sublegal lingcod (under 35 in.) released by Year and Port",
             subtitle = 'Pooled user groups'
    )

scale = max(ling_rel_port$pSublegal) / max(ling_rel_port$n)

ggplot(data = ling_rel_port) +
  geom_line(aes(x = YEAR, y = pSublegal, color = 'Proportion Sublegal lingcod')) +
  geom_line(aes(x = YEAR, y = n * scale, color = 'Total releaed Lingcod')) +
  labs(
    x = 'Year'
  ) +
  scale_y_continuous(
    name = 'Proportion of release sublegal',
    sec.axis = sec_axis(
      ~ . / scale,
      name = 'Sample size of released lingcod',
      breaks = seq(0, max(ling_rel_port$n), by = 200)
      )
  ) +
  facet_wrap(~PORT) +
  scale_color_manual(
    breaks = c('Proportion Sublegal lingcod', 'Total releaed Lingcod'),
    values = c('Proportion Sublegal lingcod' = 'black', 'Total released Lingcod' = 'red')
  ) +
  theme_minimal()


