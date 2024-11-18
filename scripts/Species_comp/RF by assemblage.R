
library(tidyverse)
library(gt)

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
get_data("O:/DSF/GOAB/R data/RF/")

rock2020$LENGTH <- rock2020$FORK_LENGTH
#Combine data
library(plyr)
rock <- do.call(rbind.fill, list(rock9195, rock9600, rock2001, rock2002, rock2003, 
                                 rock2004, rock2005, rock2006, rock2007, rock2008, 
                                 rock2009, rock2010, rock2011, rock2012, rock2013, 
                                 rock2014, rock2015, rock2016, rock2017, rock2018, 
                                 rock2019, rock2020, rock2021, rock2022))
detach(package:plyr)

rf_dat <- rock %>% 
  filter(ASSEMB != '') %>%
  mutate(USER = if_else(USER == '', 'Unknown', USER)) %>%
  filter(STATAREA != '') %>%
  filter(STATAREA >= 100000) %>%
  mutate(
    MgmtArea = case_when(
      STATAREA < 490000 & STATAREA > 440000 & !(STATAREA %in% c(485832,485902,485933,485934)) ~ "PWS",
      STATAREA %in% c(485832,485902,485933,485934,485935,486002,495831,495901,495902,
                      495931,495932,495933,495934,495935,495936,495937,495938,495939,496001,496002,
                      505831,505901,505902,505903,505904,505905,505906,505907,505908,505909,505931,505932, 
                      505933,505934) ~ "NG",
      STATAREA %in% c(495800,495832,505700,505730,505800,505832,515630,515700,515730,
                      515801,515802,515833,525600,525630,525701,525702,525703,525731,525732,525733,
                      525801,525802,525803,525804,525805,525806,525807,525832,525833,525834,535601,
                      535602,535631,535632,535633,535634,535701,535702,535703,535704,535705,535706,
                      535707,535731,535732,535733, 535734,535802,535803,535831,545601,545602,545631,
                      545632,545633,545701,545702,545703,545704,545732,545733,545734,545804,555630,
                      555701,555733) ~ "Kod",
      STATAREA %in% c(515831,515832,515901,515902,515903,515904,515905,515906,515907,
                      515908,515931,515932,515933,515934,515935,515936,515937,515938,	515939,
                      516001,516002,525831,525835,525836,525837,525901,525902,525931,	525932,
                      526002,526003,535833,535834,535901,535902,535903,535904,535905,535906,
                      535931,	535932,535933,545900) ~ "CI",
      STATAREA %in% c(555731,555732,545731,545801,545802,545803,535801,535832) ~ "AP",
      TRUE ~ 'Err'
    ),
    # if sp = 154 then species = 'DuskyDrk';
    # *{Apply Length-Weight parameters from 1992-1995 report};
    # if SP = 142 then predwt = (10**-4.61487)*length**2.90447;
    # else if SP = 145 then predwt = (10**-4.76557)*length**3.01526;
    # else if SP = 154 or sp = 172 or sp = 173 then predwt = (10**-4.19360)*length**2.64867;
    # else if assemb = 'Pelagic' then predwt = (10**-4.51687)*length**2.84619;
    # else if assemb = 'Demersal' then predwt = (10**-4.74572)*length**3.00420;	 
    # else if assemb = 'Slope' then predwt = (10**-4.58089)*length**2.80292;
    # 
    predwt = case_when(
      #is.na(WEIGHT) ~ case_when(
        SP == 142 ~ (10^-4.61487)*LENGTH^2.90447,
        SP == 145 ~ (10^-4.76557)*LENGTH^3.01526,
        SP %in% c(154, 172, 173) ~ (10^-4.19360)*LENGTH^2.64867,
        TRUE ~ case_when(
          ASSEMB == 'Pelagic' ~ (10^-4.51687)*LENGTH^2.84619,
          ASSEMB == 'Demersal' ~ (10^-4.74572)*LENGTH^3.00420,
          ASSEMB == 'Slope' ~ (10^-4.58089)*LENGTH^2.80292
        )
      # ),
      # TRUE ~ WEIGHT
    ),
    predwt2 = case_when(
      !is.na(WEIGHT) ~ WEIGHT,
      is.na(WEIGHT) & SP == 138 ~ exp(.27487^2 / 2) * exp(-11.280) * LENGTH^3.099,
      is.na(WEIGHT) & SP == 142 ~ exp(.12157^2 / 2) * exp(-10.299) * LENGTH^2.824,
      is.na(WEIGHT) & SP == 145 ~ exp(.14096^2 / 2) * exp(-11.064) * LENGTH^3.041,
      is.na(WEIGHT) & SP == 146 ~ exp(.16359^2 / 2) * exp(-10.505) * LENGTH^2.850,
      is.na(WEIGHT) & SP == 147 ~ exp(.18030^2 / 2) * exp(-9.890) * LENGTH^2.742,
      is.na(WEIGHT) & SP == 148 ~ exp(.16213^2 / 2) * exp(-11.539) * LENGTH^3.184,
      is.na(WEIGHT) & SP == 149 ~ exp(.22233^2 / 2) * exp(-9.582) * LENGTH^2.664,
      is.na(WEIGHT) & SP == 151 ~ exp(.11880^2 / 2) * exp(-9.798) * LENGTH^2.717,
      is.na(WEIGHT) & SP == 152 ~ 0.00000985 * LENGTH^3.13,
      is.na(WEIGHT) & SP == 154 ~ exp(.19606^2 / 2) * exp(-10.467) * LENGTH^2.864,
      is.na(WEIGHT) & SP == 155 ~ exp(.12016^2 / 2) * exp(-10.484) * LENGTH^2.849,
      is.na(WEIGHT) & SP == 157 ~ exp(.22283^2 / 2) * exp(-10.293) * LENGTH^2.753,
      is.na(WEIGHT) & SP == 169 ~ exp(.19222^2 / 2) * exp(-11.495) * LENGTH^3.139,
      is.na(WEIGHT) & SP == 172 ~ exp(.16436^2 / 2) * exp(-9.755) * LENGTH^2.667,
      is.na(WEIGHT) & SP == 173 ~ exp(.18960^2 / 2) * exp(-9.973) * LENGTH^2.729,
      TRUE ~ case_when(
        ASSEMB == 'Pelagic' ~ (10^-4.51687) * LENGTH^2.84619,
        ASSEMB == 'Demersal' ~ (10^-4.74572) * LENGTH^3.00420,
        ASSEMB == 'Slope' ~ (10^-4.58089) * LENGTH^2.80292,
        TRUE ~ NA_real_  # Default case if none of the conditions match
      ),
      TRUE ~ NA_real_  # Default case if none of the conditions match
    ),
    predwt3 = case_when(
      SP == 138 ~ exp(.27487^2 / 2) * exp(-11.280) * LENGTH^3.099,
      SP == 142 ~ exp(.12157^2 / 2) * exp(-10.299) * LENGTH^2.824,
      SP == 145 ~ exp(.14096^2 / 2) * exp(-11.064) * LENGTH^3.041,
      SP == 146 ~ exp(.16359^2 / 2) * exp(-10.505) * LENGTH^2.850,
      SP == 147 ~ exp(.18030^2 / 2) * exp(-9.890) * LENGTH^2.742,
      SP == 148 ~ exp(.16213^2 / 2) * exp(-11.539) * LENGTH^3.184,
      SP == 149 ~ exp(.22233^2 / 2) * exp(-9.582) * LENGTH^2.664,
      SP == 151 ~ exp(.11880^2 / 2) * exp(-9.798) * LENGTH^2.717,
      SP == 152 ~ 0.00000985 * LENGTH^3.13,
      SP == 154 ~ exp(.19606^2 / 2) * exp(-10.467) * LENGTH^2.864,
      SP == 155 ~ exp(.12016^2 / 2) * exp(-10.484) * LENGTH^2.849,
      SP == 157 ~ exp(.22283^2 / 2) * exp(-10.293) * LENGTH^2.753,
      SP == 169 ~ exp(.19222^2 / 2) * exp(-11.495) * LENGTH^3.139,
      SP == 172 ~ exp(.16436^2 / 2) * exp(-9.755) * LENGTH^2.667,
      SP == 173 ~ exp(.18960^2 / 2) * exp(-9.973) * LENGTH^2.729,
      TRUE ~ case_when(
      ASSEMB == 'Pelagic' ~ (10^-4.51687) * LENGTH^2.84619,
      ASSEMB == 'Demersal' ~ (10^-4.74572) * LENGTH^3.00420,
      ASSEMB == 'Slope' ~ (10^-4.58089) * LENGTH^2.80292,
      TRUE ~ NA_real_  # Default case if none of the conditions match
      )
    ),
    ASSEMB = case_when(
      SPECIES == 'Yelleye' ~ 'Yelloweye',
      ASSEMB == 'Slope' ~ 'Demersal',
      TRUE ~ ASSEMB
    ),
  )

# Species composition
gt(
spcomp <- rf_dat %>% 
  filter(MgmtArea == 'PWS') %>% 
  group_by(MgmtArea, YEAR, ASSEMB) %>%
  summarise(count = n()) %>%
  mutate(p = count / sum(count))
)


# Avg weights
gt(
  spcomp <- rf_dat %>% 
    filter(MgmtArea == 'PWS') %>% 
    group_by(MgmtArea, YEAR, ASSEMB) %>%
    reframe(avg_wt = mean(predwt3, na.rm = TRUE),
            avg_wt_lb = avg_wt * 2.20462)
)
