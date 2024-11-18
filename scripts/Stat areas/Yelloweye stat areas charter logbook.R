library(sf) #vector data   
library(spData) #datasets
library(tidyverse) #data manipulation
library(readxl)
library(maps)


logbook <- read_xlsx("O:/DSF/SportF/Data/Charter_logbook/Copy of 2006 - 2021 Final Saltwater Summaries.xlsx",
                     #sheet = "Bottomfish by SWHS Area", 
                     range = "Bottomfish by SWHS Area!A6:AB6732")
logbook <- logbook %>% filter(Year > 0)

logbook_public <- logbook %>% filter(Businesses > 4)


logbook_ye <- logbook %>% 
  mutate(
    YE_CPUE = (`Yelloweye Kept` + `Yelloweye Released`)/`Boat Hours`
  )

CPUE_area <- logbook_ye %>% filter(YE_CPUE > 0, `Bottomfish Stat Area` > 0) %>% 
  group_by(Year, `Bottomfish Stat Area`) %>% summarise(Mean_YE_CPUE = mean(YE_CPUE))


stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")

# world <- read_sf(system.file("shapes/world.gpkg", package="spData"))
# Canada <- world %>% filter(name_long == 'Canada')
# Canada <- st_transform(Canada, crs = 3338)


CPUE_area$STAT_AREA <- CPUE_area$`Bottomfish Stat Area`

CPUE21 <- CPUE_area %>% filter(Year == 2021)

CPUE21_areas <- CPUE21[c("STAT_AREA", "Mean_YE_CPUE")]

Only_ye_areas_2021 <- subset(stat_areas, STAT_AREA %in% CPUE21_areas$STAT_AREA)

ye_areas_21_plot_data <- merge(Only_ye_areas_2021, CPUE21_areas, by = "STAT_AREA")

ye_box_21 <- st_bbox(Only_ye_areas_2021)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
 # geom_sf(data = Canada, color = "black", fill = "grey50")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = ye_areas_21_plot_data, color = "black", aes(fill = Mean_YE_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Yelloweye Rockfish CPUE \n(no.fish/angler hour)\nfrom Charter Logbook Data 2021")+
  geom_sf_text(data = Only_ye_areas_2021, aes(label = STAT_AREA), size = 2, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(ye_box_21[1], ye_box_21[3]),
           ylim = c(ye_box_21[2], ye_box_21[4]))
