
# Creating variables indicating simply whether or not a species was encountered on a trip
  ## Looking at both Kept and Released
  ## 1 = present, 0 = absend
ye_indicator <- model_dat_w_zero %>%
  mutate(
    # Halibut
    HA_SEEN = case_when(
      HACAUGHT > 0 | HAKEPT > 0 | HAREL > 0 | HARELCIR > 0 | HARELOTH > 0 ~ 1,
      TRUE ~ 0
    ),
    # Rockfish (general)
    RF_SEEN = case_when(
      RFKEPT > 0 | RFREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Pelagic rockfish
    PEL_SEEN = case_when(
      PELKEPT > 0 | PELREL > 0 | PELRSURF > 0 | PELRDRM > 0 | PELRVENT > 0 ~ 1,
      TRUE ~ 0
    ),
    # Non-pelagic rockfish (not including yelloweye)
    NP_SEEN = case_when(
      NPKEPT > 0 | NPREL > 0 | NPRSURF > 0 | NPRDRM > 0 | NPRVENT > 0 ~ 1,
      TRUE ~ 0
    ),
    # Lingcod
    LC_SEEN = case_when(
      LCKEPT > 0 | LCREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Pacific Sleeper Shark
    PSS_SEEN = case_when(
      PSSKEPT > 0 | PSSREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Spiny Dogfish
    SDF_SEEN = case_when(
      SDFKEPT > 0 | SDFREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Salmon Shark
    SSK_SEEN = case_when(
      SSKKEPT > 0 | SSKREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Pacific Cod
    COD_SEEN = case_when(
      CODKEPT > 0 | CODREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Sablefish
    SAB_SEEN = case_when(
      SABKEPT > 0 | SABREL > 0 ~ 1,
      TRUE ~ 0
    ),
    # Pollock
    POL_SEEN = case_when(
      POLKEPT > 0 | POLREL > 0 ~ 1,
      TRUE ~ 1
    ),
    # Yelloweye
    YE_SEEN = case_when(
      YE_CAUGHT > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(YEAR, MONTH, PORT, STAT_AREA,YE_CAUGHT, YE_CPUE, YE_SEEN, HA_SEEN, RF_SEEN, PEL_SEEN,
         NP_SEEN, LC_SEEN, PSS_SEEN, SDF_SEEN, SSK_SEEN, COD_SEEN, SAB_SEEN, POL_SEEN)


## Probablities of coocurrence 

# Model for cooccurence with other species

model_all <- glm(YE_SEEN ~ HA_SEEN + PEL_SEEN + NP_SEEN + LC_SEEN + PSS_SEEN +
                   SDF_SEEN + SSK_SEEN + COD_SEEN + SAB_SEEN + POL_SEEN,
                 data = ye_indicator, family = "binomial")

# Regression coefficients
coef_ha <- coef(model_all)["HA_SEEN"]
coef_pel <- coef(model_all)["PEL_SEEN"]
coef_np <- coef(model_all)["NP_SEEN"]
coef_lc <- coef(model_all)["LC_SEEN"]
coef_pss <- coef(model_all)["PSS_SEEN"]
coef_sdf <- coef(model_all)["SDF_SEEN"]
coef_ssk <- coef(model_all)["SSK_SEEN"]
coef_cod <- coef(model_all)["COD_SEEN"]
coef_sab <- coef(model_all)["SAB_SEEN"]
# coef_pol <- coef(model_all)["POL_SEEN"] # NA, model does not converge if using just pollock as a variable
B_0 <- coef(model_all)[1]



# Assign a score for each trip as a function of species caught during that trip
## Sj = exp(sum(xij*Bi)) , where xij is the species i for trip j and Bi is the 
## predictive impact of species i


ye_scored <- ye_indicator %>% 
  mutate(
    Sj = exp(
      B_0 +
        (HA_SEEN * coef_ha) +
        (PEL_SEEN * coef_pel) +
        (NP_SEEN * coef_np) +
        (LC_SEEN * coef_lc) +
        #(PSS_SEEN * coef_pss) + # Sharks are not significantly related in the model
        #(SDF_SEEN * coef_sdf) +
        #(SSK_SEEN * coef_ssk) +
        (COD_SEEN * coef_cod) + 
        (SAB_SEEN * coef_sab) 
       # (POL_SEEN * coef_pol) # Returns na values in the model
    ),
    # Score is then converted into a probability of observing the target species
    # given the vector of presences and absences of k non-target species
    ## πj = Pr{Yj = 1} = (Sj / 1 + Sj)
    ## πj is the probablility that YE_SEEN = 1 for trip j
    πj = (Sj / (1 + Sj)),
    ## Given coefficients and presence absense indicators (X_SEEN), the log-likelihood is the sum:
    ## L{Y_SEEN |B_0, B_1,....B_K, x1j,...,xkj} = sum(log(πj)) j ∈ j+ + sum(log(1 - πj)) j ∈
    log_j_plus = case_when(
      YE_SEEN == 1 ~ log(πj),
      TRUE ~ 0
    ),
    log_j_minus = case_when(
      YE_SEEN == 0 ~ log(1 - πj),
      TRUE ~ 0
    )
  )

# Ranking each area by the average likelihood calculated to be in Yelloweye habitat
ye_scored_stat <- ye_scored %>% 
  group_by(STAT_AREA) %>% 
  reframe(avg_πj = mean(πj)) %>% 
  mutate(rank = dense_rank(desc(avg_πj))) 



## Given coefficients and presence absense indicators (X_SEEN), the log-likelihood is the sum:
## L{Y_SEEN |B_0, B_1,....B_K, x1j,...,xkj} = sum(log(πj)) j ∈ j+ + sum(log(1 - πj)) j ∈ j-

log_l_YE <- sum(ye_scored$log_j_plus) + sum(ye_scored$log_j_minus)



ye_scored_plot <- ye_scored %>% 
  group_by(πj) %>% 
  reframe(
    n = n(),
    obs_trips = sum(YE_SEEN),
    avg_πj = mean(πj),
    pred_trips = n * avg_πj,
    abs_obs_pred = abs(obs_trips - pred_trips)
  ) %>%
  mutate(percentage = n / nrow(ye_scored) * 100)

##### Binning data

ye_scored_plot_bin <- ye_scored %>%
  mutate(πj_bin = cut(πj, breaks = seq(min(πj), max(πj) + 0.01, 0.01), include.lowest = TRUE, right = TRUE)) %>%
  group_by(πj_bin) %>%
  reframe(
    n = n(),
    obs_trips = sum(YE_SEEN),
    avg_πj = mean(πj),
    pred_trips = n * avg_πj,
    abs_obs_pred = abs(obs_trips - pred_trips)
  ) %>%
  mutate(percentage = cumsum(n / nrow(ye_scored) * 100))

ggplot(data = ye_scored_plot_bin, aes(x = avg_πj, y = abs_obs_pred)) +
  geom_line() +  
  geom_line(aes(y = percentage), linetype = 2) +
  geom_point(shape = 15) +
  labs(x = "Probability", y = "Trips: abs(obs-pred)") + 
  scale_y_continuous(sec.axis = sec_axis(~.*1,name="Percentage of data selected")) +
 # geom_vline(xintercept = 0.54904201)+  # minimum difference in observed and predicted trips from binned data @ 0.05 bins
  geom_vline(xintercept = 0.51887881) + # minimum difference in observed and predicted trips from binned data @ 0.01 bins
  theme_minimal() 

# Using log on y-axis to make easier to compare, focusing around critical value
ggplot(data = ye_scored_plot_bin, aes(x = avg_πj, y = abs_obs_pred)) +
  geom_line() +  
  geom_line(aes(y = percentage), linetype = 2) +
  geom_point(shape = 15) +
  labs(x = "Probability", y = "Trips: abs(obs-pred)") + 
  scale_y_continuous(sec.axis = sec_axis(~.*1,name="Percentage of data selected")) +
  # geom_vline(xintercept = 0.54904201)+  # minimum difference in observed and predicted trips from binned data @ 0.05 bins
  #geom_vline(xintercept = 0.51887881) + # minimum difference in observed and predicted trips from binned data @ 0.01 bins
  xlim(0.41, 0.62) +
  theme_minimal() 

##

ggplot(data = ye_scored, aes(x = πj)) + # bins of 0.1
  geom_histogram(color = "black", fill =  "grey", binwidth = 0.1) +
  labs(x = "Probability", y = "Frequency") +
  #geom_vline(xintercept = 0.28668690) # minimum difference in observed and predicted trips
  #geom_vline(xintercept = 0.4006017) # minimum difference in observed and predicted trips from binned data @ 0.1 bins
  #geom_vline(xintercept = 0.54904201) # minimum difference in observed and predicted trips from binned data @ 0.05 bins
  geom_vline(xintercept = 0.51887881) + # minimum difference in observed and predicted trips from binned data @ 0.01 bins & 2nd min without bins
  theme_minimal()


# ggplot(data = ye_scored_plot, aes(x = πj, y = abs(obs_trips - pred_trips))) +
#   geom_line() +
#   labs(x = "Probability", y = "Trips: abs(obs-pred)") +
#   geom_vline(xintercept = 0.28668690)
# 
# ggplot(data = ye_scored_plot, aes(x = πj, y = percentage)) +
#   geom_line() +
#   labs(x = "Probability")

############ Fitting location data #############

# Stephens & MacCall use Y ~ location + year + season, where Y is the presence/absense of target species
   # I don't have season, this is all summer data. May try month instead

model_location <- glm(YE_SEEN ~ STAT_AREA + YEAR + MONTH,
                 data = ye_indicator, family = "binomial")

summary(model_location)

# Regression coefficients
coef_stat <- coef(model_location)["STAT_AREA"]
coef_yr <- coef(model_location)["YEAR"]
coef_month <- coef(model_location)["MONTH"]

B_0_location <- coef(model_location)[1]

ye_scored_location <- ye_indicator %>% 
  mutate(
    Sj = exp(
      B_0_location +
        (STAT_AREA * coef_stat) +
        (YEAR * coef_yr) +
       (MONTH * coef_month) 
    ),
    # Score is then converted into a probability of observing the target species
    # given the vector of presences and absences of k non-target species
    ## πj = Pr{Yj = 1} = (Sj / 1 + Sj)
    ## πj is the probablility that YE_SEEN = 1 for trip j
    πj = (Sj / (1 + Sj)),
    ## Given coefficients and presence absense indicators (X_SEEN), the log-likelihood is the sum:
    ## L{Y_SEEN |B_0, B_1,....B_K, x1j,...,xkj} = sum(log(πj)) j ∈ j+ + sum(log(1 - πj)) j ∈
    log_j_plus = case_when(
      YE_SEEN == 1 ~ log(πj),
      TRUE ~ 0
    ),
    log_j_minus = case_when(
      YE_SEEN == 0 ~ log(1 - πj),
      TRUE ~ 0
    )
  )

log_l_YE_location <- sum(ye_scored_location$log_j_plus) + sum(ye_scored_location$log_j_minus)

eq_9 <- ye_scored_location %>% 
  group_by(STAT_AREA) %>% 
  reframe(avg_πj_loc = mean(πj))

loc_ranks <- eq_9 %>% 
  mutate(rank = dense_rank(desc(avg_πj_loc))) 

spp_loc <- ye_scored_stat %>% 
  rename(avg_πj_spp = avg_πj) %>% 
  select(STAT_AREA, avg_πj_spp)

fig_5 <- merge(eq_9, spp_loc, by = "STAT_AREA")

# Comparing probabilities between location and species models
ggplot(data = fig_5, aes(x = avg_πj_loc, y = avg_πj_spp)) +
  geom_point()+
  geom_abline()+
  xlim(0,1)+
  ylim(0,1)+
  labs(x = "Probablility (location-based)", y = "Probablility (species-based)")+
  theme_minimal()

# Comparing area rankings from species method to number ranked equal or better in location method
## I don't really understand the point of this, it might make sense to compare rankngs between methods, 
## but comparing pure rankings is just odd and seems to add nothing of value
spp_rank <- ye_scored_stat %>% group_by(rank)

spp_rank_comp <- spp_rank %>%
  mutate(compare = sapply(rank, function(x) sum(loc_ranks$rank <= x)))

ggplot(data = spp_rank_comp, aes(x = rank, y = compare)) +
  geom_line() +
  #geom_abline() +
  labs(x = "Locations ordered by species ranking", y = "Ranks compared") +
  theme_minimal()


### Using CPUE data for trips selected by the critical value
crit_ye_trips <- ye_scored %>% # Retains ~ 85% of data
  filter(πj <= 0.51887881)


## Plotting CPUE
library(sf)
library(maps)


stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")

Only_ye_areas <- subset(stat_areas, STAT_AREA %in% crit_ye_trips$STAT_AREA)

crit_use <- crit_ye_trips %>% 
  group_by(STAT_AREA) %>% 
  reframe(YE_CPUE = mean(YE_CPUE))

crit_ye_trips_plot <- merge(Only_ye_areas, crit_use, by = "STAT_AREA")

ye_box <- st_bbox(Only_ye_areas)


## CPUE
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = crit_ye_trips_plot, color = "black", aes(fill = YE_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye CPUE (no.fish/angler hour) \nfrom Interview Data 2013-2023")+
  geom_sf_text(data = crit_ye_trips_plot, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))


crit_ye_year <- crit_ye_trips %>% 
  group_by(YEAR) %>% 
  reframe(YE_CPUE = mean(YE_CPUE))

ye_10_plus_year <- ye_scored %>% 
  group_by(YEAR) %>% 
  reframe(YE_CPUE = mean(YE_CPUE))

ggplot()+
  geom_line(data = crit_ye_year, aes(x = YEAR, y = YE_CPUE), linetype = 1) + 
  geom_line(data = ye_10_plus_year, aes(x = YEAR, y = YE_CPUE), linetype = 2) +
  labs(x = "YEAR", y = "Avg. Yelloweye CPUE (no. fish per angler hour)") +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) +
  theme_minimal()

###### Working with logbook data ##########

ye_indicator_logbook <- logbook %>%
  mutate(
    HA_SEEN = case_when(
      `Halibut Kept` > 0 | `Halibut Released` ~ 1,
      TRUE ~ 0
    ),
    PEL_SEEN = case_when(
      `Pelagic Rockfish Kept` > 0 | `Pelagic Rockfish Released` > 0 ~ 1,
      TRUE ~ 0
    ),
    NP_SEEN = case_when(
      `Non-Pelagic Rockfish Kept` > 0 | `Non-Pelagic Rockfish Released` > 0 ~ 1,
      TRUE ~ 0
    ),
    LC_SEEN = case_when(
      `Lingcod Kept` > 0 | `Lingcod Released` > 0 ~ 1,
      TRUE ~ 0
    ),
    SSK_SEEN = case_when(
      `Salmon Shark Kept` > 0 | `Salmon Shark Released` > 0 ~ 1,
      TRUE ~ 0
    ),
    SAB_SEEN = case_when(
      `Sablefish Kept` > 0 | `Sablefish Released` > 0 ~ 1,
      TRUE ~ 0
    ),
    YE_SEEN = case_when(
      `Yelloweye Kept` > 0 | `Yelloweye Released` > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  select(Year, `SWHS Area`, `Bottomfish Stat Area`, HA_SEEN, PEL_SEEN, NP_SEEN,
         LC_SEEN, SSK_SEEN, SAB_SEEN, YE_SEEN)


## Probablities of coocurrence 


model_all_logbook <- glm(YE_SEEN ~ HA_SEEN + PEL_SEEN + NP_SEEN + LC_SEEN +SSK_SEEN + SAB_SEEN,
                 data = ye_indicator, family = "binomial")

# Regression coefficients
coef_ha_logbook <- coef(model_all_logbook)["HA_SEEN"]
coef_pel_logbook <- coef(model_all_logbook)["PEL_SEEN"]
coef_np_logbook <- coef(model_all_logbook)["NP_SEEN"]
coef_lc_logbook <- coef(model_all_logbook)["LC_SEEN"]
coef_ssk_logbook <- coef(model_all_logbook)["SSK_SEEN"]
coef_sab_logbook <- coef(model_all_logbook)["SAB_SEEN"]
B_0_logbook <- coef(model_all_logbook)[1]




# Assign a score for each trip as a function of species caught during that trip
## Sj = exp(sum(xij*Bi)) , where xij is the species i for trip j and Bi is the 
## predictive impact of species i


ye_scored_logbook <- ye_indicator_logbook %>% 
  mutate(
    Sj = exp(
      (YE_SEEN * B_0_logbook) +
        (HA_SEEN * coef_ha_logbook) +
        (PEL_SEEN * coef_pel_logbook) +
        (NP_SEEN * coef_np_logbook) +
        (LC_SEEN * coef_lc_logbook) +
        (SSK_SEEN * coef_ssk_logbook) +
        (SAB_SEEN * coef_sab_logbook)
    ),
    # Score is then converted into a probability of observing the target species
    # given the vector of presences and absences of k non-target species
    ## πj = Pr{Yj = 1} = (Sj / 1 + Sj)
    ## πj is the probablility that YE_SEEN = 1 for trip j
    πj = (Sj / (1 + Sj)),
    ## Given coefficients and presence absense indicators (X_SEEN), the log-likelihood is the sum:
    ## L{Y_SEEN |B_0, B_1,....B_K, x1j,...,xkj} = sum(log(πj)) j ∈ j+ + sum(log(1 - πj)) j ∈
    log = case_when(
      YE_SEEN == 1 ~ log(πj),
      YE_SEEN == 0 ~ log(1 - πj)
    )
  )

## Given coefficients and presence absense indicators (X_SEEN), the log-likelihood is the sum:
## L{Y_SEEN |B_0, B_1,....B_K, x1j,...,xkj} = sum(log(πj)) j ∈ j+ + sum(log(1 - πj)) j ∈ j-

log_l_YE_logbook <- sum(ye_scored_logbook$log)



ye_scored_logbook_plot <- ye_scored_logbook %>% 
  group_by(πj) %>% 
  reframe(
    n = n(),
    obs_trips = sum(YE_SEEN),
    avg_πj = mean(πj),
    pred_trips = n * avg_πj
  )

ggplot(data = ye_scored_logbook_plot, aes(x = πj)) + # bins of 0.1
  geom_histogram(color = "black", fill =  "grey", binwidth = 0.1) +
  labs(x = "Probability", y = "Frequency")

ggplot(data = ye_scored_logbook_plot, aes(x = πj, y = abs(obs_trips - pred_trips))) +
  geom_line() +
  labs(x = "Probability", y = "Trips: abs(obs-pred)")
