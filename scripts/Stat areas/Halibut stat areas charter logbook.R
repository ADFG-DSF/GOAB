library(tidyverse)
library(readxl)
library(sf)
library(maps)

logbook <- read_xlsx("O:/DSF/SportF/Data/Charter_logbook/Copy of 2006 - 2021 Final Saltwater Summaries.xlsx",
                     #sheet = "Bottomfish by SWHS Area", 
                     range = "Bottomfish by SWHS Area!A6:AB6732") %>% filter(Year > 0)


logbook_public <- logbook %>% filter(Businesses > 4)

logbook$Hal_CPUE <- (logbook$'Halibut Kept'/logbook$'Tots Anglers')/logbook$'Boat Hours'

CPUE_area <- logbook %>% filter(Hal_CPUE > 0, `Bottomfish Stat Area` >0) %>% 
  group_by(Year, `Bottomfish Stat Area`) %>% summarise(Mean_Hal_CPUE = mean(Hal_CPUE))


stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")

# hal_22_COMBI$STAT_AREA <- hal_22_COMBI$ADFGSTATHAL
# hal_22_COMBI_areas <- hal_22_COMBI[c("STAT_AREA", "H_CPUE_COMBI")]
# 
# Only_hal_areas_COMBI <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_areas$STAT_AREA)
# 
# hal_plot_data_22_COMBI <- merge(Only_hal_areas_COMBI, hal_22_COMBI_areas, by = "STAT_AREA")
# 
# hal_box <- st_bbox(Only_hal_areas_COMBI)
# 
# pdf(here("Hal_CPUE_22.pdf"))
# ggplot()+
#   geom_sf(data = Alaska, color = "black", fill = "grey")+
#   geom_sf(data = stat_areas, color = "black", fill = "transparent")+
#   geom_sf(data = hal_plot_data_22_COMBI, color = "black", aes(fill = H_CPUE_COMBI))+
#   scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours")+
#   geom_sf_text(data = Only_hal_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
#   coord_sf(xlim = c(hal_box[1], hal_box[3]), 
#            ylim = c(hal_box[2], hal_box[4]))
# dev.off()

CPUE_area$STAT_AREA <- CPUE_area$`Bottomfish Stat Area`

CPUE21 <- CPUE_area %>% filter(Year == 2021)

CPUE21_areas <- CPUE21[c("STAT_AREA", "Mean_Hal_HCPUE")]

Only_hal_areas_2021 <- subset(stat_areas, STAT_AREA %in% CPUE21_areas$STAT_AREA)

hal_areas_21_plot_data <- merge(Only_hal_areas_2021, CPUE21_areas, by = "STAT_AREA")

hal_box_21 <- st_bbox(Only_hal_areas_2021)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_areas_21_plot_data, color = "black", aes(fill = Mean_Hal_HCPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut Harvest CPUE \n(no.fish/angler hour)\nfrom Charter Logbook Data 2021")+
 # geom_sf_text(data = Only_hal_areas_2021, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box_21[1], hal_box_21[3]),
           ylim = c(hal_box_21[2], hal_box_21[4]))
