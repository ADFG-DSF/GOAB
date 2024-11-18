library(plyr)
library(tidyverse)
library(sf)
library(maps)

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

#Data
get_data("O:/DSF/GOAB/R data/Intervw/")

stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")
detach(package:plyr)
#Working with 2023 Rockfish data
int23[is.na(int23)] <- 0
# int23$RF_CAUGHT <- int23$PELKEPT + int23$PELREL + int23$YEKEPT + int23$YEREL + int23$NPKEPT + int23$NPREL
# int23$RF_CPUE <- int23$RF_CAUGHT / int23$HOURS_R

#2023 data using RF directed stat area or combined area
int_23_rf <- int23 %>% 
  mutate(
    RF_CAUGHT = PELKEPT + PELREL + YEKEPT + YEREL + NPKEPT + NPREL,
    R_HOURS_Both = case_when(
      is.na(HOURS_R) & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      TRUE ~ HOURS_R
    ),
    R_CPUE_Both = RF_CAUGHT / R_HOURS_Both,
    STAT_AREA = case_when(
      !is.na(ADFGSTATRF) ~ ADFGSTATRF,
      is.na(ADFGSTATRF) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      TRUE ~ 0
    )
  )

rf_23 <- int_23_rf %>% filter(R_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(STAT_AREA) %>% 
  summarise(RF_CPUE = mean(R_CPUE_Both)) 





Only_rf_areas <- subset(stat_areas, STAT_AREA %in% rf_23$STAT_AREA)

rf_plot_data_23 <- merge(Only_rf_areas, rf_23, by = "STAT_AREA")

rf_box <- st_bbox(Only_rf_areas)


pdf("O:/DSF/GOAB/R Code/Figures/RF_CPUE_23.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_23, color = "black", aes(fill = RF_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours")+
  coord_sf(xlim = c(rf_box[1] - 100000, rf_box[3] + 100000), 
           ylim = c(rf_box[2] - 100000, rf_box[4] + 100000))
dev.off()

pdf("O:/DSF/GOAB/R Code/Figures/RF_CPUE_23_statareas.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_23, color = "black", aes(fill = RF_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE (no.fish/angler hour) \nfrom Interview Data 2023 \nusing rockfish hours or \ncombined hours")+
  geom_sf_text(data = Only_rf_areas, aes(label = STAT_AREA), size = 2, check_overlap = TRUE, fontface = "bold", color = "black")+
  coord_sf(xlim = c(rf_box[1] - 100000, rf_box[3] + 100000), 
           ylim = c(rf_box[2] - 100000, rf_box[4] + 100000))
dev.off()

##2023 CPUE using combined hours
int23[is.na(int23)] <- 0
int23$RF_CAUGHT <- int23$PELKEPT + int23$PELREL + int23$YEKEPT + int23$YEREL + int23$NPKEPT + int23$NPREL
int23$RF_CPUE_COMBI <- int23$RF_CAUGHT / int23$HOURS_COMBI



rf_23_COMBI <- int23 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATRF) %>% 
  summarise(RF_CPUE_COMBI = mean(RF_CPUE_COMBI)) %>% filter(ADFGSTATRF > 0)




rf_23_COMBI$STAT_AREA <- rf_23_COMBI$ADFGSTATRF
rf_23_COMBI_areas <- rf_23_COMBI[c("STAT_AREA", "RF_CPUE_COMBI")]



Only_rf_areas_COMBI <- subset(stat_areas, STAT_AREA %in% rf_23_COMBI_areas$STAT_AREA)

rf_plot_data_23_COMBI <- merge(Only_rf_areas_COMBI, rf_23_COMBI_areas, by = "STAT_AREA")

rf_COMBI_box <- st_bbox(Only_rf_areas_COMBI)


#pdf("O:/DSF/GOAB/SASCODE/R Code/RF_CPUE_23.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "grey", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_23_COMBI, color = "black", aes(fill = RF_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE \n(no.fish/angler hour)\nfrom Interview Data 2023 \nusing combined hours")+
  geom_sf_text(data = Only_rf_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(rf_COMBI_box[1], rf_COMBI_box[3]), 
           ylim = c(rf_COMBI_box[2], rf_COMBI_box[4]))

#dev.off()

##########################################################################################

#Working with 2022 Rockfish data
int22[is.na(int22)] <- 0
int22$RF_CAUGHT <- int22$PELKEPT + int22$PELREL + int22$YEKEPT + int22$YEREL + int22$NPKEPT + int22$NPREL
int22$RF_CPUE <- int22$RF_CAUGHT / int22$HOURS_R

rf_22 <- int22 %>% filter(HOURS_R > 0) %>% group_by(ADFGSTATRF) %>% 
  summarise(RF_CPUE = mean(RF_CPUE)) %>% filter(ADFGSTATRF > 0)


rf_22$STAT_AREA <- rf_22$ADFGSTATRF
rf_22_areas <- rf_22[c("STAT_AREA", "RF_CPUE")]

Only_rf_areas <- subset(stat_areas, STAT_AREA %in% rf_22_areas$STAT_AREA)

rf_plot_data_22 <- merge(Only_rf_areas, rf_22_areas, by = "STAT_AREA")

rf_box <- st_bbox(Only_rf_areas)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_22, color = "black", aes(fill = RF_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE (no.fish/angler hour) \nfrom Interview Data 2022")+
  coord_sf(xlim = c(rf_box[1] - 100000, rf_box[3] + 100000), 
           ylim = c(rf_box[2] - 100000, rf_box[4] + 100000))

##2022 CPUE using combined hours
int22[is.na(int22)] <- 0
int22$RF_CAUGHT <- int22$PELKEPT + int22$PELREL + int22$YEKEPT + int22$YEREL + int22$NPKEPT + int22$NPREL
int22$RF_CPUE_COMBI <- int22$RF_CAUGHT / int22$HOURS_COMBI



rf_22_COMBI <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATRF) %>% 
  summarise(RF_CPUE_COMBI = mean(RF_CPUE_COMBI)) %>% filter(ADFGSTATRF > 0)




rf_22_COMBI$STAT_AREA <- rf_22_COMBI$ADFGSTATRF
rf_22_COMBI_areas <- rf_22_COMBI[c("STAT_AREA", "RF_CPUE_COMBI")]



Only_rf_areas_COMBI <- subset(stat_areas, STAT_AREA %in% rf_22_COMBI_areas$STAT_AREA)

rf_plot_data_22_COMBI <- merge(Only_rf_areas_COMBI, rf_22_COMBI_areas, by = "STAT_AREA")

rf_COMBI_box <- st_bbox(Only_rf_areas_COMBI)


pdf("O:/DSF/GOAB/SASCODE/R Code/RF_CPUE_22.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "grey", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_22_COMBI, color = "black", aes(fill = RF_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours")+
  geom_sf_text(data = Only_rf_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(rf_COMBI_box[1], rf_COMBI_box[3]), 
           ylim = c(rf_COMBI_box[2], rf_COMBI_box[4]))

dev.off()

#2021 CPUE using combined hours
int21[is.na(int21)] <- 0
int21$RF_CAUGHT <- int21$PELKEPT + int21$PELREL + int21$YEKEPT + int21$YEREL + int21$NPKEPT + int21$NPREL
int21$HOURS_COMBI <- int21$HOURS_H + int21$HOURS_L + int21$HOURS_R + int21$HOURS_TOT
int21$RF_CPUE_COMBI <- int21$RF_CAUGHT / int21$HOURS_COMBI



rf_21_COMBI <- int21 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATRF) %>% 
  summarise(RF_CPUE_COMBI = mean(RF_CPUE_COMBI)) %>% filter(ADFGSTATRF > 0)




rf_21_COMBI$STAT_AREA <- rf_21_COMBI$ADFGSTATRF
rf_21_COMBI_areas <- rf_21_COMBI[c("STAT_AREA", "RF_CPUE_COMBI")]



Only_rf_areas_COMBI <- subset(stat_areas, STAT_AREA %in% rf_21_COMBI_areas$STAT_AREA)

rf_plot_data_21_COMBI <- merge(Only_rf_areas_COMBI, rf_21_COMBI_areas, by = "STAT_AREA")

rf_COMBI_box <- st_bbox(Only_rf_areas_COMBI)


pdf("O:/DSF/GOAB/SASCODE/R Code/RF_CPUE_21.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "grey", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = rf_plot_data_21_COMBI, color = "black", aes(fill = RF_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Rockfish CPUE \n(no.fish/angler hour)\nfrom Interview Data 2021 \nusing combined hours")+
  geom_sf_text(data = Only_rf_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(rf_COMBI_box[1], rf_COMBI_box[3]), 
           ylim = c(rf_COMBI_box[2], rf_COMBI_box[4]))

dev.off()












