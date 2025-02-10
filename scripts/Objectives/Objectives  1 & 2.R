#################################################################################
# Objectives 1 & 2 - 
# Estimate the mean net weight (Obj. 1) and length (Obj. 2) of halibut taken by each 
# user group (charter / non-charter) in each subarea of Southcentral Alaska (Kodiak, Lower
# Cook Inlet, Central Cook Inlet, North Gulf, Eastern Prince William Sound, and Western
# Prince William Sound), such that the mean weight estimates for each user group in each 
# subarea are within 20% of the true mean wieght at leat 90% of the time.
# 
# This program generates harvest biomass preliminary estimates based on harvest projections.
# 
# Halibut mean weight estimated by user group. Harvest for Lower Cook Inlet (Homer sample) is stratified by fish cleaned
# at sea (CS or CAS) and fish cleaned in port (CP).
# Mean length, net weight, and round weight for each port are NO LONGER ROUNDED 
# before multiplying by the number of fish, so there may be rounding errors in the final estimates that will have to be explained.
#
# Transitioned from SAS by CWM on 02/03/25
# This was originally a huge mess, doing a first step for both objective 1 and 2 together
# then following up with only Objective 1 and no work on Objective 2
# This script coalesces all steps for objective 1 and objective 2 to a single script - CWM
################################################################################

# Part 1. Estimate the proportion of the halibut that was cleaned (and carcasses discarded) 
# at sea at each port.  These estimates are necessary to stratify estimates of mean weight
# and length composition (Objectives 1 and 2) at homer and may be useful in evaluating the 
# potential bias of estimates at other ports due to cleaning at sea.

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
                                int18, int19, int20, int21, int22, int23))
detach(package:plyr)


## Estimate the proportion of Homer halibut cleaned at sea by year to stratify the estimated
# mean weight and length of charter halibut landed in Homer

Hom <- int %>% 
  filter(
    PORT == 'Homer',
    USER == 'Charter'
  ) 

CAS_yr_Hom <- Hom %>% 
  group_by(YEAR) %>% 
  reframe(
    SUMCAS = sum(HACAS, na.rm = TRUE),
    SUMKEPT = sum(HAKEPT, na.rm = TRUE)
  ) %>% 
  mutate(
    PHAT = SUMCAS / SUMKEPT, # Op plan equation 9
    VPHAT = PHAT * (1 - PHAT) / (SUMKEPT - 1) # Op plan equation 10
  )

gt(
  CAS_yr_Hom
) %>% 
  tab_header (title = 'Proportion of Homer charter halibut havest cleaned at sea')

# Estiate the proportion of halibut cleaned at sea by port and user group

PercentCAS <- int %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    SUMCAS = sum(HACAS, na.rm = TRUE),
    SUMKEPT = sum(HAKEPT, na.rm = TRUE),
    PCAS = SUMCAS / SUMKEPT * 100
  )

gt(
  PercentCAS
 ) %>% 
  tab_header(title = 'Percent of halibut cleaned at sea by port and user group')

# Mean proportion of halibut cleaned at sea for vessel trips with halibut cleaned at sea only
fracCAS <- int %>% 
  filter(
    HACAS > 0
  ) %>% 
  mutate(
    PCAS = HACAS / HAKEPT
  ) %>% 
  group_by(PORT, USER, YEAR) %>% 
  reframe(
    Mean_pCAS = mean(PCAS)
  )

gt(fracCAS) %>% 
  tab_header(title = 'For trips with halibut cleaned at sea, the mean proportion that were cleaned at sea')


# Part 2.  Estimate the mean weights.  Use CAS_yr_Hom to apply 
# to stratifying the charter samples for an accurate mean charter weight in Homer.

get_data("data/Hal/")


###
library(plyr)
hal <- do.call(rbind.fill, list(hal9499, hal2000, hal2001, hal2002, hal2003, hal2004,
                                hal2005, hal2006, hal2007, hal2008, hal2009, hal2010,
                                hal2011, hal2012, hal2013, hal2014, hal2015, hal2016,
                                hal2017, hal2018, hal2019, hal2020, hal2021, hal2022,
                                hal2023))
detach(package:plyr)



means <- hal %>% 
  filter(
    !is.na(LENGTH) # only include records with lengths so freq is the sample size
    ) %>% 
  group_by(YEAR, PORT, USER) %>% 
  reframe(
    MEANCM = mean(LENGTH, na.rm = TRUE),
    MEANNET = mean(NETWT, na.rm = TRUE),
    MEANRND = mean(RNDWT, na.rm = TRUE),
    SECM = stderr(LENGTH),
    SENET = stderr(NETWT),
    SERND = stderr(RNDWT)
  )

meanwt <- means %>% 
  mutate(
    VARCM = SECM^2,
    VARNET = SENET^2,
    VARRND = SERND^2
  )

gt(meanwt) %>% 
  tab_header(title = 'Mean length and wt of halibut by port and user (before combining Homer data)')

# Homer: Combine Homer charter CAS and CIP data into a single record for charter data
Hom_hal <- meanwt %>% 
  filter(
    PORT == 'Homer',
    USER %in% c('HomCPort', 'HomCSea')
  ) %>% 
  select(
    -SECM,
    -SENET,
    -SERND
  )

CIP_hom <- Hom_hal %>% 
  filter(
    USER == 'HomCPort'
  ) %>% 
  mutate(
    NCP = n(),
    CPCM = MEANCM,
    CPNET = MEANNET,
    CPRND = MEANRND,
    VCPCM = VARCM,
    VCPNET = VARNET,
    VCPRND = VARRND,
    USER = 'Charter'
  ) %>% 
  select(
    -MEANCM,
    -MEANNET,
    -MEANRND,
    -VARCM,
    -VARNET,
    -VARRND
  )

CAS_hom <- Hom_hal %>% 
  filter(
    USER == 'HomCSea'
  ) %>% 
  mutate(
    NCS = n(),
    CSCM = MEANCM,
    CSNET = MEANNET,
    CSRND = MEANRND,
    VCSCM = VARCM,
    VCSNET = VARNET,
    VCSRND = VARRND,
    USER = 'Charter'
  ) %>% 
  select(
    -MEANCM,
    -MEANNET,
    -MEANRND,
    -VARCM,
    -VARNET,
    -VARRND
  )

Hom_hal_c <- merge(CIP_hom, CAS_hom, by = c('YEAR', 'PORT', 'USER'))

# Incorportate the interview estimates of charter CAS proportions

Hom_hal_merge <- merge(Hom_hal_c, CAS_yr_Hom, by = 'YEAR') %>% 
  mutate(
    N = NCP + NCS,
    # Mean length and weight from report equation 4a
    MEANCM = (CSCM * PHAT) + (CPCM * (1 - PHAT)),
    MEANNET = (CSNET * PHAT) + (CPNET * (1 - PHAT)),
    MEANRND = (CSRND * PHAT) + (CPRND * (1 - PHAT)),
    # Variance of mean length and weight from report equation 7
    Term1CM = (CSCM^2 * VPHAT + VCSCM * PHAT^2 - VCSCM * VPHAT),
    Term2CM = VCPCM,
    Term3CM = (CPCM^2 * VPHAT + VCPCM * (1 - PHAT)^2 - VCPCM * VPHAT),
    Term4CM = CSCM * CPCM * VPHAT,
    Term5CM = PHAT * VCPCM,
    VARCM = Term1CM + Term2CM + Term3CM - (2 * Term4CM) - (2 * Term5CM),
    
    Term1NET = (CSNET^2 * VPHAT + VCSNET * PHAT^2 - VCSNET * VPHAT),
    Term2NET = VCPNET,
    Term3NET = (CPNET^2 * VPHAT + VCPNET * (1 - PHAT)^2 - VCPNET * VPHAT),
    Term4NET = CSNET * CPNET * VPHAT,
    Term5NET = PHAT * VCPNET,
    VARNET = Term1NET + Term2NET + Term3NET - (2 * Term4NET) - (2 * Term5NET),
    
    Term1RND = (CSRND^2 * VPHAT + VCSRND * PHAT^2 - VCSRND * VPHAT),
    Term2RND = VCPRND,
    Term3RND = (CPRND^2 * VPHAT + VCPRND * (1 - PHAT)^2 - VCPRND * VPHAT),
    Term4RND = CSRND * CPRND * VPHAT,
    Term5RND = PHAT * VCPRND,
    VARRND = Term1RND + Term2RND + Term3RND - (2 * Term4RND) - (2 * Term5RND),
    
    SECM = sqrt(abs(VARCM)),
    SENET = sqrt(abs(VARNET)),
    SERND = sqrt(abs(VARRND))
  )


gt(
  Hom_hal_merge %>% 
    select(Term1NET, Term2NET, Term3NET, Term4NET, Term5NET, VARNET)
  ) %>% 
  tab_header(title = 'Homer Variance Terms')

Hom_hal_c_2 <- Hom_hal_merge %>% 
  select(YEAR, PORT, USER, CSCM, CPCM, CSNET, CPNET, CSRND, CPRND, MEANCM, MEANNET,
         MEANRND, SECM, SENET, SERND, VARCM, VARNET, VARRND, PHAT, VPHAT)

# Add the Homer charter data back into the MEANWT file
meanwt2 <- meanwt %>% 
  filter(
    !(USER %in% c('HomCPort', 'HomCSea'))
    )

meanwt3 <- bind_rows(meanwt2, Hom_hal_c_2) %>% 
  mutate(
    # Approx. 95% confidence interval for mean wt (Objective is for this to be < 0.20)
    RPNET = 1.96 * SENET / MEANNET,
    # 95% confidence interval for length (Objective is for this to be < 0.20)
    RPCM = 1.96 * SECM / MEANCM
  ) %>% 
  select(
    -CPCM,
    -CPNET,
    -CPRND,
    -CSCM,
    -CSNET,
    -CSRND,
    -PHAT,
    -VPHAT
  )

## Objective 1
gt(
  meanwt3 %>% 
    select(YEAR, PORT, USER, MEANNET, SENET, VARNET, RPNET)
) %>% 
  tab_header(
    title = 'Net Weight: With merged PWS and Homer estimates',
    subtitle = 'RPNET is approx. relative precision on mean net wt (95% conf.)'
  )


## Objective 2
gt(
  meanwt3 %>% 
    select(YEAR, PORT, USER, MEANCM, SECM, VARCM, RPCM)
) %>% 
  tab_header(
    title = 'Length: With merged PWS and Homer estimates',
    subtitle = 'RPCM is approx. relative precision on mean length (95% conf.)'
  )
