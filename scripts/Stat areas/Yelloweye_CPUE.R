library(tidyverse)
library(sf)
library(maps)


int23 <- read.csv("O:/DSF/GOAB/R data/Intervw/int23.csv")

stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")
# Calculate CPUE
ye23 <- int23 %>% filter(YEKEPT > 0 | YERSURF > 0 | YERDRM > 0) %>% 
  mutate(
    YE_CAUGHT = YEKEPT + YERSURF + YERDRM,
    YE_HOURS_Both = case_when(
      HOURS_R == 0 & HOURS_COMBI!= 0 ~ HOURS_COMBI,
      TRUE ~ HOUR_TOT
    ),
    # Number of Hours fished X Number of Anglerdays (divide by total days as that is factored in to angler days)
    TOT_HOURS = (ANGLDAYS * YE_HOURS_Both)/NUMDAYS,
    # Fish caught per hour per angler
    YE_CPUE = YE_CAUGHT / TOT_HOURS,
    log_YE_CPUE = log(YE_CAUGHT) / TOT_HOURS,
    STAT_AREA = case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      TRUE ~ 0
    ),
    SFmgmtarea = case_when(
      STAT_AREA > 440000 & STAT_AREA < 480000 | STAT_AREA %in% c(485430, 485500, 485530, 485600,
                                                              485630, 485700, 485730, 485800, 485831, 485901, 485931, 485932, 485935,
                                                              486001, 486002, 486003, 486004, 486005, 486031, 486032, 486033, 486034,
                                                              486100) ~ 'PWS',
      STAT_AREA %in% c(485832, 485902, 485933, 485934, 485935, 486002, 495831, 495901, 495902,
                      495931, 495932, 495933, 495934, 495935, 495936, 495937, 495938, 495939, 496001, 496002,
                      505831, 505901, 505902, 505903, 505904, 505905, 505906, 505907, 505908, 505909, 505931, 505932,
                      505933, 505934) ~ 'NG',
      STAT_AREA %in% c(495800, 495832, 505700, 505730, 505800, 505832, 515630, 515700, 515730,
                      515801, 515802, 515833, 525600, 525630, 525701, 525702, 525703, 525731, 525732, 525733,
                      525801, 525802, 525803, 525804, 525805, 525806, 525807, 525832, 525833, 525834, 535601,
                      535602, 535631, 535632, 535633, 535634, 535701, 535702, 535703, 535704, 535705, 535706,
                      535707, 535731, 535732, 535733, 535734, 535802, 535803, 535831, 545601, 545602, 545631,
                      545632, 545633, 545701, 545702, 545703, 545704, 545732, 545733, 545734, 545804, 555630,
                      555701, 555733) ~ 'KOD',
      STAT_AREA %in% c(555731, 555732, 545731, 545801, 545802, 545803, 535801, 535832) ~ 'AKPen',
      STAT_AREA %in% c(515831, 515832, 515901, 515902, 515903, 515904, 515905, 515906, 515907,
                      515908, 515931, 515932, 515933, 515934, 515935, 515936, 515937, 515938, 515939,
                      516001, 516002, 525831, 525835, 525836, 525837, 525901, 525902, 525931, 525932,
                      526002, 526003, 535833, 535834, 535901, 535902, 535903, 535904, 535905, 535906,
                      535931, 535932, 535933, 545900) ~ 'CI',
    )
  ) %>% 
  filter(YE_HOURS_Both != 0) %>% 
  select(PORT, USER, YEKEPT, YERSURF, YERDRM, YE_CAUGHT, YE_CPUE, log_YE_CPUE, YE_HOURS_Both, TOT_HOURS, ANGLDAYS, NUMDAYS, STAT_AREA, SFmgmtarea)


ye_23_dat <- ye23 %>% filter(YE_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(STAT_AREA) %>% 
  summarise(YE_CPUE = mean(YE_CPUE),
            log_YE_CPUE = mean(log_YE_CPUE),
            n = n()) 

Only_ye_areas <- subset(stat_areas, STAT_AREA %in% ye_23_dat$STAT_AREA)

ye_plot_data_23 <- merge(Only_ye_areas, ye_23_dat, by = "STAT_AREA")

ye_box <- st_bbox(Only_ye_areas)


## CPUE
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23, color = "black", aes(fill = YE_CPUE, alpha = n))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours")+
  geom_sf_text(data = ye_plot_data_23, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))

## log CPUE
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23, color = "black", aes(fill = log_YE_CPUE, alpha = n))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye log CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours")+
  geom_sf_text(data = ye_plot_data_23, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))

## log CPUE with number of observations
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23, color = "black", aes(fill = log_YE_CPUE, alpha = n))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye log CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours \nwith number of observations (interviews)")+
  geom_sf_text(data = ye_plot_data_23, aes(label = n), size = 2.5, check_overlap = TRUE)+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))

## By User
ye_23_dat_USER <- ye23 %>% filter(YE_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(USER, STAT_AREA) %>% 
  summarise(YE_CPUE = mean(YE_CPUE)) 

Only_ye_areas_USER <- subset(stat_areas, STAT_AREA %in% ye_23_dat_USER$STAT_AREA)

ye_plot_data_23_USER <- merge(Only_ye_areas_USER, ye_23_dat_USER, by = "STAT_AREA")

ye_box <- st_bbox(Only_ye_areas_USER)



ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23_USER, color = "black", aes(fill = YE_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours \nby User")+
  geom_sf_text(data = ye_plot_data_23_USER, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))+
  facet_wrap(~USER)

## By Port
ye_23_dat_PORT <- ye23 %>% filter(YE_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(PORT, STAT_AREA) %>% 
  summarise(YE_CPUE = mean(YE_CPUE)) 

Only_ye_areas_PORT <- subset(stat_areas, STAT_AREA %in% ye_23_dat_PORT$STAT_AREA)

ye_plot_data_23_PORT <- merge(Only_ye_areas_PORT, ye_23_dat_PORT, by = "STAT_AREA")

ye_box <- st_bbox(Only_ye_areas_PORT)



ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23_PORT, color = "black", aes(fill = YE_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours \nby Port")+
  geom_sf_text(data = ye_plot_data_23_PORT, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))+
  facet_wrap(~PORT)

## By SFmgmtarea
ye_23_dat_SFmgmtarea <- ye23 %>% filter(YE_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(SFmgmtarea, STAT_AREA) %>% 
  summarise(YE_CPUE = mean(YE_CPUE)) 

Only_ye_areas_SFmgmtarea <- subset(stat_areas, STAT_AREA %in% ye_23_dat_SFmgmtarea$STAT_AREA)

ye_plot_data_23_SFmgmtarea <- merge(Only_ye_areas_SFmgmtarea, ye_23_dat_SFmgmtarea, by = "STAT_AREA")

ye_box <- st_bbox(Only_ye_areas_SFmgmtarea)



ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_plot_data_23_SFmgmtarea, color = "black", aes(fill = YE_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours \n by Mgmt area")+
  geom_sf_text(data = ye_plot_data_23_SFmgmtarea, aes(label = STAT_AREA), size = 2.5, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box[1] - 100000, ye_box[3] + 100000), 
           ylim = c(ye_box[2] - 100000, ye_box[4] + 100000))+
  facet_wrap(~SFmgmtarea)


