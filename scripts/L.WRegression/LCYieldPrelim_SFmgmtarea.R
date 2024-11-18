############
# STILL HAVE SOME PROBLEM WITH 2010 SOUTHEAST REGION ESTIMATES!
#   ;
# 
# 
# 
# /*************************************************************************************************************************
#   RFYieldPrelim_bySpecies.sas - prelim estimates of rockfish yield based on species comp from AWL sampling, 1991 and up.
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
# Used by MDS 3/13/19 to create management area specific estimates of yield for 2019 area review.
# Used by MDS 3/17/19 to create lingcod code for lingcod yield by year and management area
# Used by MDS 9/23/19 to get lingcod yield for seward fish 1992-2019 by port of landing
#
# Translated from SAS to R by CWM on 06/19/23
##########################################################################


library(tidyverse)


get_data <- function(a) {
  
  
  print(a)
  
  files <- list.files(path=a,
                      pattern="*.csv", full.names=F, recursive=FALSE)
  
  
  for(i in seq(1, length(files))) {
    print(files[[i]])
    n <- gsub(".csv", "",files[[i]])
    assign(n, read.csv(paste0(a,files[[i]])), envir = .GlobalEnv)
    
  }
  
}

#Function call
get_data("O:/DSF/GOAB/R data/LC/")

#####

library(plyr)

lc <- do.call(rbind.fill, list(ling9104, ling2005, ling2006, ling2007, ling2008, ling2009, ling2010, ling2011,
                                  ling2012, ling2013, ling2014, ling2015, ling2016, ling2017,
                                  ling2018, ling2019, ling2020, ling2021, ling2022))


detach(package:plyr)



# Modify the 'rf' dataframe using mutate() and case_when()
lc <- lc %>%
  mutate(USER = if_else(USER == '', 'Unknown', USER),
         PORT = case_when(PORT %in% c('CCI', 'Cordova') ~ NA_character_, #Not enough data to use, unsure how to weight Cordova data
                          PORT == 'Whittier' & YEAR == 1991 ~ 'PWS', #Valdez and Whittier pooled for 1991 because harvest not broken out by E/W until 1999
                          PORT == 'Valdez' & YEAR %in% 1991:1998 ~ 'PWS', #Let Valdez represent PWS 1992 through 1998. Estimate biomass separately for EPWS (Valdez) and WPWS (Whittier) starting in 1999.
                          TRUE ~ PORT),
         SP = 130,
         SPECIES = 'Lingcod')

# Create LWsample dataframe for sample sizes by species
LWsample <- lc %>%
  filter(!is.na(LENGTH) & !is.na(WEIGHT)) %>%
  group_by(SPECIES, PORT) %>%
  summarise(N = n()) %>%
  ungroup()

# Create frequency table of sample sizes
LWfreqtable <- LWsample %>%
  #count(SPECIES, PORT, name = 'N') %>%
  complete(SPECIES, PORT, fill = list(N = 0)) %>%
  mutate(TOTAL = sum(N)) %>%
  mutate(PERCENT = (N / TOTAL) * 100) %>%
  select(SPECIES, PORT, N, PERCENT)


# Filter data based on condition
LWsample_filter <- LWsample %>% filter(N >= 30)
LWsample_plot <- lc %>% filter(!is.na(LENGTH), !is.na(WEIGHT))

# Create scatter plots
ggplot(LWsample_plot, aes(x = WEIGHT, y = LENGTH, color = factor(YEAR), alpha = 0.2)) +
  geom_point() +
  labs(x = "Weight", y = "Length", legend = "Year") + 
  theme_minimal()



# Create PREDWT2 variable
lc2 <- lc %>% filter(!is.na(LENGTH)) %>% 
  mutate(predwt2 = exp(0.141169**2/2) * exp(-10.657) * LENGTH**2.820)  # Calculate predwt2 column

lc2 <- lc2 %>% 
  arrange(PORT, YEAR)  # Sort the data by port and year


MeanWt <- lc2 %>% 
  group_by(PORT, YEAR) %>%  # Group the data by port and year
  summarise(across(predwt2, list(mean = mean, SE = ~sd(.)/sqrt(length(.)))), .groups = 'drop') %>% 
  rename(meankg2 = predwt2_mean, SEkg2 = predwt2_SE)


MeanWt <- MeanWt %>%
  mutate(meanlb2 = meankg2 * 2.20462, SElb2 = SEkg2 * 2.20462)  # Calculate meanlb2 and SElb2 columns

#Delete rows where port is empty
 MeanWt <- MeanWt %>% filter(PORT != "")

# If you want to round meankg and SEkg columns
# MeanWt <- MeanWt %>% mutate(meankg = round(meankg, 0.1), SEkg = round(SEkg, 0.01))






library(readxl)

R2SWHS <- read_xlsx('O:/DSF/GOAB/Harvest/Prelim LC yield/R2_SWHS91-17_SFmgmtarea_LC.xlsx', sheet = 'R2_SWHS91-15')

R2SWHS <- R2SWHS %>%
  filter(!(Port == 'PWS' & Year >= 1999)) %>% 
  mutate(
    SFmgmtarea = Port,
    YEAR = Year,
  vHarv = SEharv^2
) %>% 
  select(-Port, -Year)

MeanWt <- MeanWt %>% mutate(
  SFmgmtarea = case_when(
    PORT %in% c('Whittier', 'Valdez') ~ 'PWS',
    PORT %in% c('Homer', 'CCI') ~ 'CI',
    PORT == 'Kodiak' ~ 'KOD',
    PORT == 'Seward' ~ 'NG',
    TRUE ~ 'PWS'
  )
) %>% 
  select(-PORT)

Yield <- merge(MeanWt, R2SWHS, by = c("SFmgmtarea", "YEAR"))


Yield <- Yield %>%
  filter(!is.na(SFmgmtarea)) %>%  # Remove rows with empty port
  filter(YEAR > 1995)  # Remove rows with year less than or equal to 1995

Yield <- Yield %>%
  mutate(Yieldkg2 = Harvest * meankg2,
         vYield2 = Harvest^2 * SEkg2^2 + vHarv * meankg2^2 - vHarv * SEkg2^2,
         SEYieldkg2 = sqrt(vYield2))
