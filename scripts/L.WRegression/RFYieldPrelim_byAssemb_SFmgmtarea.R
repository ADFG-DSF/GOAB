############
# RFYieldPrelim_bySpecies.sas - prelim estimates of rockfish yield based on species comp from AWL sampling, 1991 and up.
# 
# Estimates are preliminary because they are based on L-W regressions that heven't yet been tested for differences
# 	between years, ports, sexes, etc.
# This code uses species-specific L-W regressions when possible, and generic assemblage regressions for species
# 	without adequate data to construct a L-W regression.
# Species proportions are available only from AWL sampling and may be biased by sampler preference for
# 	uncommon species, easy to sample species, etc. In addition, fish returning to port may not be a accurate
# 	representation of the species comp of the harvest - for example, many private anglers in PWS could be cleaning
# 	rockfish at sea, and returning only to port with larger fish (yelloweye), while charters bring in everything.
# 	These estimates are not stratified by user group because harvest not stratified by user group before about 1996
# 	and standard errors for harvest by user group are only available since 2003. 
# See alternate estimates using assemblage composition from interviews or logbooks should be computed
# 	(e.g., RFYieldPrelim_byAssemb.sas).
# 
# Computation is based on the basic formula: Yield(s) = Harvest(all s) * SpeciesComp(s) * MeanWt(s).
# 	These are summed over various ADF&G management areas or NMFS areas, also by mgmt assemblage or NMFS SAFE category.
# 	Does not stratify by user - writing program to produce NMFS estimates that are. 
# 
# Special considerations:
# 	No rockfish sampling in Kodiak in 1991, so to calculate NMFS Central GOA yield in 1991 I had to assume an average 
# 	weight and species comp for each SAFE category (e.g. based on subsequent years). See section labeled "Add Kodiak 1991 data;"
# 
# 	No sampling in Western GOA - estimates could be ginned up substituting species comp and mean weight from Kodiak.
# 	
# 	
# 
# 
# SCM 09/06/13
# MDS 12/20/18 Using this code in order to get yield in pounds by ADFG rockfish assemblage, mainly to separate out yelloweye
# from the non-pelagics for AMR.
#
# Translated from SAS to R by CWM on 06/19/23
##########################################################################


library(tidyverse)


source("functions.R")

#Function call
get_data("data/RF/")



#####

library(plyr)
rf <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                     rock2004, rock2005, rock2006, rock2007, rock2008, 
                                     rock2009, rock2010, rock2011, rock2012, rock2013, 
                                     rock2014, rock2015, rock2016, rock2017, rock2018, 
                                     rock2019, rock2020, rock2021, rock2022))
species <- character()
abbrev <- character()

detach(package:plyr)




rf <- rf %>%
  mutate(SFmgmtarea = case_when(
    STATAREA < 10000 ~ NA_character_,
    as.integer(STATAREA / 10000) >= 44 & as.integer(STATAREA / 10000) < 49 & !STATAREA %in% c(485832, 485902, 485933, 485934) ~ 'PWS',
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
                    516001, 516002, 525831, 525835, 525836, 525837, 525901, 525902, 525931,
                    525932, 526002, 526003, 535833, 535834, 535901, 535902, 535903, 535904, 535905, 535906,
                    535931, 535932, 535933, 545900) ~ 'CI',
    is.na(STATAREA) & PORT == 'Homer' ~ 'CI',
    is.na(STATAREA) & PORT == 'Kodiak' ~ 'KOD',
    is.na(STATAREA) & PORT == 'Whittier' ~ 'PWS',
    is.na(STATAREA) & PORT == 'Valdez' ~ 'PWS',
    TRUE ~ NA_character_
  ),
    USER = if_else(USER == '', 'Unknown', USER),
         PORT = case_when(PORT %in% c('CCI', 'Cordova') ~ NA_character_, #Not enough data to use, unsure how to weight Cordova data
                          PORT == 'Whittier' & YEAR == 1991 ~ 'PWS', #Valdez and Whittier pooled for 1991 because harvest not broken out by E/W until 1999
                          PORT == 'Valdez' & YEAR %in% 1991:1998 ~ 'PWS', #Let Valdez represent PWS 1992 through 1998. Estimate biomass separately for EPWS (Valdez) and WPWS (Whittier) starting in 1999.
                          TRUE ~ PORT), #Do NOT subset to exclude fish with missing lengths - they are still used to estimate assemblage composition
         SPECIES = case_when(SP == 154 ~ 'DuskyDrk',
                             TRUE ~ SPECIES),
         PREDWT = case_when(SP == 142 ~ (10 ** -4.61487) * LENGTH ** 2.90447, #Apply Length-Weight parameters from 1992-1995 report
                            SP == 145 ~ (10 ** -4.76557) * LENGTH ** 3.01526,
                            SP %in% c(154, 172, 173) ~ (10 ** -4.19360) * LENGTH ** 2.64867,
                            ASSEMB == 'Pelagic' ~ (10 ** -4.51687) * LENGTH ** 2.84619,
                            ASSEMB == 'Demersal' ~ (10 ** -4.74572) * LENGTH ** 3.00420,
                            ASSEMB == 'Slope' ~ (10 ** -4.58089) * LENGTH ** 2.80292,
                            TRUE ~ NA_real_))

# Create LWsample dataframe for sample sizes by species
LWsample <- rf %>%
  filter(!is.na(LENGTH) & !is.na(WEIGHT)) %>%
  group_by(SPECIES, SFmgmtarea) %>%
  summarise(N = n()) %>%
  ungroup()

# Create frequency table of sample sizes
LWfreqtable <- LWsample %>%
  count(SPECIES, SFmgmtarea, name = 'N') %>%
  complete(SPECIES, SFmgmtarea, fill = list(N = 0)) %>%
  mutate(TOTAL = sum(N)) %>%
  mutate(PERCENT = (N / TOTAL) * 100) %>%
  select(SPECIES, SFmgmtarea, N, PERCENT)

# Calculate sample size by species
sampsize <- rf %>%
  group_by(SPECIES) %>%
  summarise(TOTN = n()) %>%
  ungroup()


# Merge sample size with LWsample
LWsample <- LWsample %>%
  left_join(sampsize, by = "SPECIES")

# Filter data based on condition
LWsample_filter <- LWsample %>% filter(TOTN >= 30)
LWsample_plot <- rf %>% filter(SPECIES %in% LWsample_filter$SPECIES, !is.na(LENGTH), !is.na(WEIGHT))

# Create scatter plots
ggplot(LWsample_plot, aes(x = WEIGHT, y = LENGTH, color = factor(YEAR), alpha = 0.2)) +
  geom_point() +
  facet_wrap(~ SPECIES) +
  labs(x = "Weight", y = "Length", legend = "Year") + 
  theme_minimal()


# Basic L-W regressions without any GLM to test for differences between SFmgmtareas/years/users.
# #   Includes all data (no outliers excluded)
# library(broom)
# 
# # Create logarithmic variables
# LWsample_log <- rf %>% 
#   filter(!is.na(LENGTH), !is.na(WEIGHT), !is.na(SP),
#          is.numeric(LENGTH), is.numeric(LENGTH)) %>% 
#   mutate(lnTL = log(LENGTH),
#          lnWt = log(WEIGHT))
# 
# # Sort data by 'sp'
# LWsample_log <- LWsample_log %>%
#   arrange(SPECIES)
# 
# LWsample_log$LENGTH <- as.numeric(LWsample_log$LENGTH)
# LWsample_log$WEIGHT <- as.numeric(LWsample_log$WEIGHT)
# # Perform linear regression by 'sp'
# reg_results <- LWsample_log %>%
#   group_by(SPECIES) %>%
#   filter(SPECIES %in% LWsample_filter$SPECIES) %>% 
#   do(model_fit = lm(lnWt ~ lnTL, data = .)) %>%
#   tidy(model_fit, exponentiate = TRUE)
# 
# # Extract regression coefficients
# LWparam <- reg_results %>%
#   select(SPECIES, term, estimate) %>%
#   spread(term, estimate)

#######################
# Provide alternate mean weight (predwt2) using regressions based on larger sample sizes and
# ln-ln transformed l and w, relationships for data through 2009
# 
# {Sources for parameter estimates \data\rfish\LN-WT\rflw.sas}

# Create PREDWT2 variable
rf2 <- rf %>%
  mutate(PREDWT2 = case_when(
    SP == 138 ~ exp(0.27487^2/2) * exp(-11.280) * LENGTH^3.099,
    SP == 142 ~ exp(0.12157^2/2) * exp(-10.299) * LENGTH^2.824,
    SP == 145 ~ exp(0.14096^2/2) * exp(-11.064) * LENGTH^3.041,
    SP == 146 ~ exp(0.16359^2/2) * exp(-10.505) * LENGTH^2.850,
    SP == 147 ~ exp(0.18030^2/2) * exp(-9.890) * LENGTH^2.742,
    SP == 148 ~ exp(0.16213^2/2) * exp(-11.539) * LENGTH^3.184,
    SP == 149 ~ exp(0.22233^2/2) * exp(-9.582) * LENGTH^2.664,
    SP == 151 ~ exp(0.11880^2/2) * exp(-9.798) * LENGTH^2.717,
    SP == 152 ~ 0.00000985 * LENGTH^3.13,
    SP == 154 ~ exp(0.19606^2/2) * exp(-10.467) * LENGTH^2.864,
    SP == 155 ~ exp(0.12016^2/2) * exp(-10.484) * LENGTH^2.849,
    SP == 157 ~ exp(0.22283^2/2) * exp(-10.293) * LENGTH^2.753,
    SP == 169 ~ exp(0.19222^2/2) * exp(-11.495) * LENGTH^3.139,
    SP == 172 ~ exp(0.16436^2/2) * exp(-9.755) * LENGTH^2.667,
    SP == 173 ~ exp(0.18960^2/2) * exp(-9.973) * LENGTH^2.729,
    ASSEMB == "Pelagic" ~ (10**-4.51687) * LENGTH^2.84619,
    ASSEMB == "Demersal" ~ (10**-4.74572) * LENGTH^3.00420,
    ASSEMB == "Slope" ~ (10**-4.58089) * LENGTH^2.80292,
    TRUE ~ NA_real_
  ))

# Sort the data
rf2 <- rf2 %>%
  arrange(SFmgmtarea, YEAR, SP)

# Calculate means and standard errors
meanWt <- rf2 %>%
  filter(!is.na(PREDWT)) %>% 
  group_by(SFmgmtarea, YEAR, SP) %>%
  summarise(
    nmeas = sum(PREDWT, na.rm = TRUE),
    meankg = mean(c(PREDWT, PREDWT2, LENGTH, AGE), na.rm = TRUE),
    meankg2 = mean(PREDWT2, na.rm = TRUE),
    meanlength = mean(LENGTH, na.rm = TRUE),
    meanage = mean(AGE, na.rm = TRUE),
    SEkg = sd(c(PREDWT, PREDWT2), na.rm = TRUE) / sqrt(sum(!is.na(PREDWT))),
    SEkg2 = sd(PREDWT2, na.rm = TRUE) / sqrt(sum(!is.na(PREDWT2)))
  ) %>%
  ungroup()

meanWt <- meanWt %>%
  mutate(
    meanlb = meankg * 2.20462,
    meanlb2 = meankg2 * 2.20462,
    SElb = SEkg * 2.20462,
    SElb2 = SEkg2 * 2.20462
  )

# Add Kodiak 1991 data - see note at top of program and spreadsheet Kodiak1991Problem.xlsx:
#   Species comp for 1991 is based on average of 1992-1994 excluding yellowtail and unspecified
# demersals. Mean weight for 1991 is based on average of 1992-1994.

Kod1991meanwt <- data.frame(
  SFmgmtarea = c("Kodiak", "Kodiak", "Kodiak"),
  YEAR = c(1991, 1991, 1991),
  SP = c(142, 145, 154),
  meankg = c(1.76538, 3.70551, 1.37831),
  meankg2 = c(1.80729, 3.79651, 1.41187),
  SEkg = c(0.02218, 0.40666, 0.03913),
  SEkg2 = c(0.02212, 0.41992, 0.04302),
  meanlb = c(3.89199, 8.16925, 3.03866),
  meanlb2 = c(3.98438, 8.36985, 3.11264),
  SElb = c(0.04890, 0.89653, 0.08628),
  SElb2 = c(0.04876, 0.92577, 0.09485)
)

#Combine Kodiak 1991 and other SFmgmtarea data
meanWtcombi <- meanWt %>% select(SFmgmtarea, YEAR, SP, meankg, meankg2, SEkg, SEkg2, meanlb, meanlb2, SElb, SElb2)
meanWtcombi <- as.data.frame(meanWtcombi)
meanWtcombi$YEAR <- as.numeric(meanWtcombi$YEAR)
meanWtcombi$SP <- as.numeric(meanWtcombi$SP)
meanWtcombi <- full_join(meanWtcombi, Kod1991meanwt, by = c('SFmgmtarea', 'YEAR', 'SP', 'meankg', 'meankg2', 'SEkg', 
                                                            'SEkg2', 'meanlb', 'meanlb2', 'SElb', 'SElb2'))
# meanWt <- meanWtcombi %>%
#   filter(SFmgmtarea == "Seward" & SP == 145)


##Black and yelloweye mean weight by SFmgmtarea and year
meanWtcombi <- meanWtcombi %>%
  arrange(SP, SFmgmtarea, YEAR)

print(meanWtcombi %>% filter(SP == 142), nrow = Inf)
print(meanWtcombi %>% filter(SP == 145), nrow = Inf)

##Get species composition from AWL data (okay if length data missing)
# Sort the data by SFmgmtarea and year

rf2 <- rf2 %>% arrange(SFmgmtarea, YEAR)
rf2$YEAR <- as.numeric(rf$YEAR)

rf3 <- rf2 %>%
  group_by(SFmgmtarea, YEAR, SP) %>%
  summarize(Total = n()) %>%
  group_by(SFmgmtarea, YEAR) %>%
  mutate(Percent = Total / sum(Total) * 100) %>%
  ungroup()
# Calculate species composition frequencies
Spcomp <- rf3 %>%
  group_by(SFmgmtarea, YEAR) %>%
  ungroup() %>%
  mutate(p = Percent / 100,
         vp = p * (1 - p) / (Total - 1),
         SEp = sqrt(vp)) %>% 
  select(-Total, - Percent)
# Create the Kod1991sp data frame
Kod1991sp <- data.frame(
  SFmgmtarea = c("Kod", "Kod", "Kod"),
  YEAR = c(1991, 1991, 1991),
  SP = c(142, 145, 154),
  p = c(0.752, 0.011, 0.237),
  vp = c(0.000339, 0.000018, 0.000308),
  SEp = c(0.0184, 0.0042, 0.0176)
)

# Merge with Kod1991sp data
Spcomp <- full_join(Spcomp, Kod1991sp, by = c("SFmgmtarea", "YEAR", "SP", "p", "vp", "SEp"))
Spcomp <- Spcomp %>%
  arrange(SFmgmtarea, YEAR, SP)


library(readxl)

R2SWHS <- read_xlsx('O:/DSF/GOAB/Harvest/Prelim RF yield/R2_SWHS91-17.xlsx', sheet = 'R2_SWHS91-15')

R2SWHS <- R2SWHS %>%
  mutate(
    SFmgmtarea = case_when(
      Port %in% c('Whittier', 'Valdez') ~ 'PWS',
      Port == 'Seward' ~ 'NGC',
      Port %in% c('Homer', 'CCI') ~ 'CI',
      Port == 'Kodiak' ~ 'KOD',
      TRUE ~ 'PWS'
    ),
    YEAR = Year,
  vHarv = SEharv^2
) %>% 
  filter(!(SFmgmtarea == 'PWS' & Year <= 1996)) %>% 
  select(-Port, -Year)


Yield <- merge(meanWtcombi, Spcomp, by = c("SFmgmtarea", "YEAR", "SP"))
Yield <- merge(Yield, R2SWHS, by = c("SFmgmtarea", "YEAR"))

Yield <- Yield %>% mutate(
  HarvSp = Harvest * p,
  vHsp = Harvest^2 * vp + vHarv * p^2 - vHarv * vp,
  Yieldkg = HarvSp * meankg,
  vYield = HarvSp^2 * SEkg^2 + vHsp * meankg^2 - vHsp * SEkg^2,
  SEYieldkg = sqrt(vYield),
  Yieldkg2 = HarvSp * meankg2,
  vYield2 = HarvSp^2 * SEkg2^2 + vHsp * meankg2^2 - vHsp * SEkg2^2,
  SEYieldkg2 = sqrt(vYield2)
)

# Yield <- Yield %>% filter(SFmgmtarea == 'Seward' & SP == 145)

##Sum ADF&G sSFmgmtarea fish assemblages
assemb <- Yield %>%
  mutate(ASSEMB = case_when(
    SP %in% c(142, 154, 172, 173, 156, 155, 169) ~ "Pelagic",
    SP == 145 ~ "Yelloweye",
    TRUE ~ "Non-pel"
  )) %>%
  select(SFmgmtarea, ASSEMB, YEAR, HarvSp, vHsp, Yieldkg, vYield, Yieldkg2, vYield2) %>%
  arrange(SFmgmtarea, ASSEMB, YEAR)


YieldAssemb <- assemb %>%
  group_by(SFmgmtarea, ASSEMB, YEAR) %>%
  summarise(
    Harv = sum(HarvSp),
    vHarv = sum(vHsp),
    Yieldkg = sum(Yieldkg),
    vYield = sum(vYield),
    Yieldkg2 = sum(Yieldkg2),
    vYield2 = sum(vYield2)
  ) %>%
  mutate(
    SEHarv = sqrt(vHarv),
    SEYield = sqrt(vYield),
    SEYield2 = sqrt(vYield2),
    Pounds = Yieldkg * 2.20462,
    SElb = sqrt(vYield) * 2.20462,
    Pounds2 = Yieldkg2 * 2.20462,
    SElb2 = sqrt(vYield2) * 2.20462
  ) %>%
  select(-vHarv, -vYield, -vYield2)

