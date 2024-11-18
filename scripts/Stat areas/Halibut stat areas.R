library(here)
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
#Working with 2023 Halibut data
int23[is.na(int23)] <- 0
int23$H_CAUGHT <- int23$HAKEPT + int23$HAREL
int23$H_CPUE <- int23$H_CAUGHT / int23$HOURS_H

hal_23 <- int23 %>% filter(HOURS_H > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE = mean(H_CPUE)) %>% filter(ADFGSTATHAL > 0)


hal_23$STAT_AREA <- hal_23$ADFGSTATHAL
hal_23_areas <- hal_23[c("STAT_AREA", "H_CPUE")]

Only_hal_areas <- subset(stat_areas, STAT_AREA %in% hal_23_areas$STAT_AREA)

hal_plot_data_23 <- merge(Only_hal_areas, hal_23_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_23, color = "black", aes(fill = H_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE (no.fish/angler hour) \nfrom Interview Data 2023")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000))+
  theme(axis.title = element_blank())

##2023 CPUE using combined hours
int23[is.na(int23)] <- 0
int23$H_CAUGHT <- int23$HAKEPT + int23$HAREL
int23$H_CPUE_COMBI <- int23$H_CAUGHT / int23$HOURS_COMBI


hal_23_COMBI <- int23 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)


hal_23_COMBI$STAT_AREA <- hal_23_COMBI$ADFGSTATHAL
hal_23_COMBI_areas <- hal_23_COMBI[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI <- subset(stat_areas, STAT_AREA %in% hal_23_COMBI_areas$STAT_AREA)

hal_plot_data_23_COMBI <- merge(Only_hal_areas_COMBI, hal_23_COMBI_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI)

#pdf(here("Hal_CPUE_23.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_23_COMBI, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2023 \nusing combined hours")+
  geom_sf_text(data = Only_hal_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
#dev.off()

# 2023 Using Combined hours when no halibut hours

int23_cpue <- int23 %>% 
  mutate(
    H_HOURS_Both = case_when(
      is.na(HOURS_H) & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      TRUE ~ HOURS_H
    ),
    H_CPUE_Both = H_CAUGHT / H_HOURS_Both,
    STAT_AREA = case_when(
      !is.na(ADFGSTATHAL) ~ ADFGSTATHAL,
      is.na(ADFGSTATHAL) & !is.na(ADFGSTATCOMBI) ~ ADFGSTATCOMBI,
      TRUE ~ 0
    )
  )



hal_23_both <- int23_cpue %>% filter(H_HOURS_Both > 0, STAT_AREA != 0) %>% group_by(STAT_AREA) %>% 
  summarise(H_CPUE_BOTH = mean(H_CPUE_Both, na.rm = TRUE))



Only_hal_areas_both <- subset(stat_areas, STAT_AREA %in% hal_23_both$STAT_AREA)

hal_plot_data_23_both <- merge(Only_hal_areas_both, hal_23_both, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_both)

pdf("O:/DSF/GOAB/R Code/Figures/Hal_CPUE_23_statareas.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_23_both, color = "black", aes(fill = H_CPUE_BOTH))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2023 \nusing halibut hours or \ncombined hours")+
  geom_sf_text(data = Only_hal_areas_both, aes(label = STAT_AREA), size = 2, check_overlap = TRUE, fontface = "bold", color = "black")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

pdf("O:/DSF/GOAB/R Code/Figures/Hal_CPUE_23.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_23_both, color = "black", aes(fill = H_CPUE_BOTH))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2023 \nusing halibut hours or \ncombined hours")+
  #geom_sf_text(data = Only_hal_areas_both, aes(label = STAT_AREA), size = 2, check_overlap = TRUE, fontface = "bold", color = "black")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()
###########################################################################################

#Working with 2022 Halibut data
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE <- int22$H_CAUGHT / int22$HOURS_H

hal_22 <- int22 %>% filter(HOURS_H > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE = mean(H_CPUE)) %>% filter(ADFGSTATHAL > 0)


hal_22$STAT_AREA <- hal_22$ADFGSTATHAL
hal_22_areas <- hal_22[c("STAT_AREA", "H_CPUE")]

Only_hal_areas <- subset(stat_areas, STAT_AREA %in% hal_22_areas$STAT_AREA)

hal_plot_data_22 <- merge(Only_hal_areas, hal_22_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22, color = "black", aes(fill = H_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE (no.fish/angler hour) \nfrom Interview Data 2022")+
  coord_sf(xlim = c(hal_box[1] - 100000, hal_box[3] + 100000), 
           ylim = c(hal_box[2] - 100000, hal_box[4] + 100000))+
  theme(axis.title = element_blank())

##2022 CPUE using combined hours
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)


hal_22_COMBI$STAT_AREA <- hal_22_COMBI$ADFGSTATHAL
hal_22_COMBI_areas <- hal_22_COMBI[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_areas$STAT_AREA)

hal_plot_data_22_COMBI <- merge(Only_hal_areas_COMBI, hal_22_COMBI_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI)

pdf(here("Hal_CPUE_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours")+
  geom_sf_text(data = Only_hal_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()


##2021 CPUE using combined hours
int21[is.na(int21)] <- 0
int21$H_CAUGHT <- int21$HAKEPT + int21$HAREL
int21$HOURS_COMBI <- int21$HOURS_H + int21$HOURS_L + int21$HOURS_R + int21$HOURS_TOT
int21$H_CPUE_COMBI <- int21$H_CAUGHT / int21$HOURS_COMBI

hal_21_COMBI <- int21 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)


hal_21_COMBI$STAT_AREA <- hal_21_COMBI$ADFGSTATHAL
hal_21_COMBI_areas <- hal_21_COMBI[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI <- subset(stat_areas, STAT_AREA %in% hal_21_COMBI_areas$STAT_AREA)

hal_plot_data_21_COMBI <- merge(Only_hal_areas_COMBI, hal_21_COMBI_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI)

pdf(here("Hal_CPUE_21.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_21_COMBI, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2021 \nusing combined hours")+
  geom_sf_text(data = Only_hal_areas_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()


#2022 by port, Homer
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_port <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_HOMER <- hal_22_COMBI_port %>% filter(PORT == "Homer")


hal_22_COMBI_HOMER$STAT_AREA <- hal_22_COMBI_HOMER$ADFGSTATHAL
hal_22_COMBI_HOMER_areas <- hal_22_COMBI_HOMER[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI_HOMER <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_HOMER_areas$STAT_AREA)

hal_plot_data_22_COMBI_HOMER <- merge(Only_hal_areas_COMBI_HOMER, hal_22_COMBI_HOMER_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI_HOMER)

pdf(here("Hal_CPUE_Homer_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI_HOMER, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours \ncaught in Homer, AK")+
  geom_sf_text(data = Only_hal_areas_COMBI_HOMER, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

#2022 by port, Valdez
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_port <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_Valdez <- hal_22_COMBI_port %>% filter(PORT == "Valdez")


hal_22_COMBI_Valdez$STAT_AREA <- hal_22_COMBI_Valdez$ADFGSTATHAL
hal_22_COMBI_Valdez_areas <- hal_22_COMBI_Valdez[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI_Valdez <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_Valdez_areas$STAT_AREA)

hal_plot_data_22_COMBI_Valdez <- merge(Only_hal_areas_COMBI_Valdez, hal_22_COMBI_Valdez_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI_Valdez)

pdf(here("Hal_CPUE_Valdez_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI_Valdez, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours \ncaught in Valdez, AK")+
  geom_sf_text(data = Only_hal_areas_COMBI_Valdez, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

#2022 by port, Kodiak
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_port <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_Kodiak <- hal_22_COMBI_port %>% filter(PORT == "Kodiak")


hal_22_COMBI_Kodiak$STAT_AREA <- hal_22_COMBI_Kodiak$ADFGSTATHAL
hal_22_COMBI_Kodiak_areas <- hal_22_COMBI_Kodiak[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI_Kodiak <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_Kodiak_areas$STAT_AREA)

hal_plot_data_22_COMBI_Kodiak <- merge(Only_hal_areas_COMBI_Kodiak, hal_22_COMBI_Kodiak_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI_Kodiak)

pdf(here("Hal_CPUE_Kodiak_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI_Kodiak, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours \ncaught in Kodiak, AK")+
  geom_sf_text(data = Only_hal_areas_COMBI_Kodiak, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

#2022 by port, Seward
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_port <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_Seward <- hal_22_COMBI_port %>% filter(PORT == "Seward")


hal_22_COMBI_Seward$STAT_AREA <- hal_22_COMBI_Seward$ADFGSTATHAL
hal_22_COMBI_Seward_areas <- hal_22_COMBI_Seward[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI_Seward <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_Seward_areas$STAT_AREA)

hal_plot_data_22_COMBI_Seward <- merge(Only_hal_areas_COMBI_Seward, hal_22_COMBI_Seward_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI_Seward)

pdf(here("Hal_CPUE_Seward_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI_Seward, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours \ncaught in Seward, AK")+
  geom_sf_text(data = Only_hal_areas_COMBI_Seward, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

#2022 by port, Whittier
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_port <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(PORT, ADFGSTATHAL) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_Whittier <- hal_22_COMBI_port %>% filter(PORT == "Whittier")


hal_22_COMBI_Whittier$STAT_AREA <- hal_22_COMBI_Whittier$ADFGSTATHAL
hal_22_COMBI_Whittier_areas <- hal_22_COMBI_Whittier[c("STAT_AREA", "H_CPUE_COMBI")]

Only_hal_areas_COMBI_Whittier <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_Whittier_areas$STAT_AREA)

hal_plot_data_22_COMBI_Whittier <- merge(Only_hal_areas_COMBI_Whittier, hal_22_COMBI_Whittier_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_COMBI_Whittier)

pdf(here("Hal_CPUE_Whittier_22.pdf"))
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = hal_plot_data_22_COMBI_Whittier, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours \ncaught in Whittier, AK")+
  geom_sf_text(data = Only_hal_areas_COMBI_Whittier, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  theme(axis.title = element_blank())
dev.off()

#2022 CPUE Charter vs. Private
int22[is.na(int22)] <- 0
int22$H_CAUGHT <- int22$HAKEPT + int22$HAREL
int22$H_CPUE_COMBI <- int22$H_CAUGHT / int22$HOURS_COMBI

hal_22_COMBI_user <- int22 %>% filter(HOURS_COMBI > 0) %>% group_by(ADFGSTATHAL, USER) %>% 
  summarise(H_CPUE_COMBI = mean(H_CPUE_COMBI)) %>% filter(ADFGSTATHAL > 0)

hal_22_COMBI_user$STAT_AREA <- hal_22_COMBI_user$ADFGSTATHAL

hal_22_COMBI_user_areas <- hal_22_COMBI_user[c("STAT_AREA", "USER", "H_CPUE_COMBI")]

Only_hal_areas_user_COMBI <- subset(stat_areas, STAT_AREA %in% hal_22_COMBI_user_areas$STAT_AREA)

hal_plot_data_22_user_COMBI <- merge(Only_hal_areas_user_COMBI, hal_22_COMBI_user_areas, by = "STAT_AREA")

hal_box <- st_bbox(Only_hal_areas_user_COMBI)

pdf(here("Hal_CPUE_by_user_22.pdf"))
ggplot()+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = hal_plot_data_22_user_COMBI, color = "black", aes(fill = H_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours")+
 # geom_sf_text(data = Only_hal_areas_user_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(hal_box[1], hal_box[3]), 
           ylim = c(hal_box[2], hal_box[4]))+
  facet_grid(cols = vars(USER))+
  theme(axis.title = element_blank())
dev.off()

#Homer Harvest CPUE
int22[is.na(int22)] <- 0
int22$H_HCPUE <- int22$HAKEPT / int22$HOURS_H

hal_22_Homer <- int22 %>% filter(HOURS_H > 0, PORT == "Homer") %>% group_by(ADFGSTATHAL) %>% 
  summarise(H_HCPUE = mean(H_HCPUE)) %>% filter(ADFGSTATHAL > 0)


hal_22_Homer$STAT_AREA <- hal_22_Homer$ADFGSTATHAL
hal_22_Homer_areas <- hal_22_Homer[c("STAT_AREA", "H_HCPUE")]

Only_hal_areas_Homer <- subset(stat_areas, STAT_AREA %in% hal_22_Homer_areas$STAT_AREA)

hal_plot_data_22_Homer <- merge(Only_hal_areas_Homer, hal_22_Homer_areas, by = "STAT_AREA")

hal_box_Homer <- st_bbox(Only_hal_areas_Homer)

pdf("O:/DSF/GOAB/SASCODE/R Code/Hal_HCPUE_Homer_22.pdf")
ggplot()+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = hal_plot_data_22_Homer, color = "black", aes(fill = H_HCPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut Harvest CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nout of Homer")+
  geom_sf_text(data = Only_hal_areas_Homer, aes(label = STAT_AREA), size = 2, check_overlap = TRUE)+
  coord_sf(xlim = c(hal_box_Homer[1], hal_box_Homer[3]), 
           ylim = c(hal_box_Homer[2], hal_box_Homer[4]),
           crs = 3338)+
  theme(axis.title = element_blank())
dev.off()

int21[is.na(int21)] <- 0
int21$H_HCPUE <- int21$HAKEPT / int21$HOURS_H

hal_21_Homer <- int21 %>% filter(HOURS_H > 0, PORT == "Homer") %>% group_by(ADFGSTATHAL) %>% 
  summarise(H_HCPUE = mean(H_HCPUE)) %>% filter(ADFGSTATHAL > 0)


hal_21_Homer$STAT_AREA <- hal_21_Homer$ADFGSTATHAL
hal_21_Homer_areas <- hal_21_Homer[c("STAT_AREA", "H_HCPUE")]

Only_hal_areas_Homer <- subset(stat_areas, STAT_AREA %in% hal_21_Homer_areas$STAT_AREA)

hal_plot_data_21_Homer <- merge(Only_hal_areas_Homer, hal_21_Homer_areas, by = "STAT_AREA")

hal_box_Homer <- st_bbox(Only_hal_areas_Homer)

pdf("O:/DSF/GOAB/SASCODE/R Code/Hal_HCPUE_Homer_21.pdf")
ggplot()+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = hal_plot_data_21_Homer, color = "black", aes(fill = H_HCPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Halibut Harvest CPUE \n(no.fish/angler hour)\nfrom Interview Data 2021 \nout of Homer")+
  geom_sf_text(data = Only_hal_areas_Homer, aes(label = STAT_AREA), size = 2, check_overlap = TRUE)+
  coord_sf(xlim = c(hal_box_Homer[1], hal_box_Homer[3]), 
           ylim = c(hal_box_Homer[2], hal_box_Homer[4]),
           crs = 3338)+
  theme(axis.title = element_blank())
dev.off()

