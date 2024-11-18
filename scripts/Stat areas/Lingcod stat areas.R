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

#Function call
get_data("O:/DSF/GOAB/R data/Intervw/")


###
int_data <- data.frame()  # create empty data frame for int_data

intervw9219$TARGET <- intervw9219$Target

int21$YEAR <- 2021 #matching ling areas to work with targetted ling areas from previous rears
int21$ADFGSTAT <- int21$ADFGSTATLING
int22$YEAR <- 2022
int22$ADFGSTAT <- int22$ADFGSTATLING
int23$YEAR <- 2023
int23$ADFGSTAT <- int23$ADFGSTATLING

int_data <- do.call(rbind.fill, list(int05, int06, int07, int08, int09, int10,
                                     int11, int12, int13, int14, int15, int16, 
                                     int16prelim, int17, int18, int19, int20, 
                                     int21, int22, int23, int9204, intervw9219 ))


# Interview criteria specified in 2003 Lingcod report
Ling_data <- int_data %>% filter(TARGET %in% c('L', 'B', 'B+S') & MONTH >= 7)
#Ling_data <- int_data %>% filter(LCCAUGHT >= 1 | LCREL >= 1) #Filter for only Lingcod Caught




Ling_by_port_kpt <- Ling_data %>%  group_by(PORT) %>% #All Lingcod kept in series by port
  summarise(LCKEPT = sum(LCKEPT, na.rm = TRUE))

Ling_data$LCTOT <- Ling_data$LCKEPT + Ling_data$LCREL #Kept and Released

##Bar chart for Lingcod caught by port with Ling target in whole time series
Ling_by_port <- Ling_data %>%  group_by(PORT) %>% 
  summarise(LC_Caught = sum(LCTOT, na.rm = TRUE))

ggplot(data = Ling_by_port)+
  geom_col(aes(x = PORT, y = LC_Caught))
####

# ggplot(data = Ling_data, aes(x=PORT)) +
#   geom_bar()

Ling_areas <- Ling_data %>% group_by(ADFGSTAT) %>% summarise(LC_CAUGHT = sum(LCTOT))
Ling_areas$STAT_AREA <- Ling_areas$ADFGSTAT
LC_areas <- Ling_areas[c("STAT_AREA", "LC_CAUGHT")]

#Map
stat_areas <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                      "State_Stat_Area_Simple")
Alaska <- read_sf("C:/Users/cwmckean/OneDrive - State of Alaska/Desktop/R Files/_AlaskaMPAs/",
                  "Alaska_Simple")
Only_ling_areas <- subset(stat_areas, STAT_AREA %in% Ling_areas$ADFGSTAT)

Ling_plot_data <- merge(stat_areas, LC_areas, by = "STAT_AREA")

LC_box <- st_bbox(Only_ling_areas)

#Total caught by area in whole time period
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data, color = "black", aes(fill = LC_CAUGHT))+
  scale_fill_distiller(palette = "Spectral", "Lingcod Caught in \nInterview Data \n1992-2023")+
  #geom_sf(data = Only_ling_areas, color = "black", fill = "red")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000))
#CPUE
Ling_data_hrs <- Ling_data %>% 
  mutate(
    HOURS_FISHED = case_when(
      !is.na(HOURS_L) ~ HOURS_L,
      is.na(HOURS_L) & !is.na(HOURS_COMBI) ~ HOURS_COMBI,
      is.na(HOURS_L) & !is.na(HOURS_TOT) ~ HOURS_TOT,
      is.na(HOURS_L) & !is.na(WHOLEHRS) ~ WHOLEHRS,
      TRUE ~ BOATHRS
    )
  ) %>%
  filter(HOURS_FISHED > 0) %>% 
  mutate(
    LC_CPUE = LCTOT / HOURS_FISHED
  ) %>% 
  select(YEAR, PORT, USER, LC_CPUE, HOURS_FISHED, HOURS_COMBI, HOURS_L, HOURS_TOT, HOURS_R, HOURS_H, BOATHRS, WHOLEHRS, LCTOT, ADFGSTAT)

Ling_CPUE <- Ling_data_hrs %>% group_by(ADFGSTAT) %>% 
  summarise(LC_CPUE = mean(LC_CPUE, na.rm = TRUE)) %>% 
  filter(!is.na(ADFGSTAT)) %>% 
  rename(STAT_AREA = ADFGSTAT)

Ling_CPUE_plot <- merge(stat_areas, Ling_CPUE, by = "STAT_AREA") %>% filter(!is.na(LC_CPUE))
LC_box <- st_bbox(Ling_CPUE_plot)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_CPUE_plot, color = "black", aes(fill = LC_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE (no.fish/angler hour) \nfrom Interview Data 2000-2023")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000))

#By Port
Ling_CPUE_port <- Ling_data_hrs %>% group_by(PORT, ADFGSTAT) %>% 
  summarise(LC_CPUE = mean(LC_CPUE, na.rm = TRUE)) %>% 
  filter(!is.na(ADFGSTAT)) %>% 
  rename(STAT_AREA = ADFGSTAT)

Ling_CPUE_port_plot <- merge(stat_areas, Ling_CPUE_port, by = "STAT_AREA") %>% filter(!is.na(LC_CPUE))


ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_CPUE_port_plot, color = "black", aes(fill = LC_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE (no.fish/angler hour) \nfrom Interview Data 2000-2023")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000)) + 
  facet_wrap(~PORT)

#By User

Ling_CPUE_USER <- Ling_data_hrs %>% group_by(USER, ADFGSTAT) %>% 
  summarise(LC_CPUE = mean(LC_CPUE, na.rm = TRUE)) %>% 
  filter(!is.na(ADFGSTAT)) %>% 
  rename(STAT_AREA = ADFGSTAT)

Ling_CPUE_USER_plot <- merge(stat_areas, Ling_CPUE_USER, by = "STAT_AREA") %>% filter(!is.na(LC_CPUE))


ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_CPUE_USER_plot, color = "black", aes(fill = LC_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE (no.fish/angler hour) \nfrom Interview Data 2000-2023")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000)) + 
  facet_wrap(~USER)

##########################################################
##2023 Data only, CPUE
int23[is.na(int23)] <- 0
int23$LC_CAUGHT <- int23$LCKEPT + int23$LCREL
int23$LC_CPUE <- int23$LC_CAUGHT / int23$HOURS_L

Ling_23 <- int23 %>% filter(HOURS_L > 0) %>%  group_by(ADFGSTATLING) %>% 
  summarise(LC_CPUE = mean(LC_CPUE)) %>% filter(ADFGSTATLING > 0)
Ling_23$STAT_AREA <- Ling_23$ADFGSTATLING
Ling_23_areas <- Ling_23[c("STAT_AREA", "LC_CPUE")]

Ling_plot_data_23 <- merge(stat_areas, Ling_23_areas, by = "STAT_AREA")

LC_box <- st_bbox(Ling_plot_data_23)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data_23, color = "black", aes(fill = LC_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE (no.fish/angler hour) \nfrom Interview Data 2023")+
  #geom_sf(data = Only_ling_areas, color = "black", fill = "red")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000))

###2023 CPUE using combined hours
int23[is.na(int23)] <- 0
int23$LC_CAUGHT <- int23$LCKEPT + int23$LCREL
int23$LC_CPUE_COMBI <- int23$LC_CAUGHT / int23$HOURS_COMBI

Ling_23_COMBI <- int23 %>% filter(HOURS_COMBI > 0) %>%  group_by(ADFGSTATLING) %>% 
  summarise(LC_CPUE_COMBI = mean(LC_CPUE_COMBI)) %>% filter(ADFGSTATLING > 0)
Ling_23_COMBI$STAT_AREA <- Ling_23_COMBI$ADFGSTATLING
Ling_23_COMBI_areas <- Ling_23_COMBI[c("STAT_AREA", "LC_CPUE_COMBI")]

Ling_plot_data_23_COMBI <- merge(stat_areas, Ling_23_COMBI_areas, by = "STAT_AREA")

LC_box <- st_bbox(Ling_plot_data_23_COMBI)

pdf("O:/DSF/GOAB/R Code/Figures/LC_CPUE_23.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data_23_COMBI, color = "black", aes(fill = LC_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE \n(no.fish/angler hour)\nfrom Interview Data 2023 \nusing combined hours")+
  geom_sf_text(data = Ling_plot_data_23_COMBI, aes(label = STAT_AREA), size = 2, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(LC_box[1], LC_box[3]), 
           ylim = c(LC_box[2], LC_box[4]))

dev.off()

######################################################################################################################################################
##2022 Data only, CPUE
int22[is.na(int22)] <- 0
int22$LC_CAUGHT <- int22$LCKEPT + int22$LCREL
int22$LC_CPUE <- int22$LC_CAUGHT / int22$HOURS_L

Ling_22 <- int22 %>% filter(HOURS_L > 0) %>%  group_by(ADFGSTATLING) %>% 
  summarise(LC_CPUE = mean(LC_CPUE)) %>% filter(ADFGSTATLING > 0)
Ling_22$STAT_AREA <- Ling_22$ADFGSTATLING
Ling_22_areas <- Ling_22[c("STAT_AREA", "LC_CPUE")]

Ling_plot_data_22 <- merge(stat_areas, Ling_22_areas, by = "STAT_AREA")

LC_box <- st_bbox(Ling_plot_data_22)

ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data_22, color = "black", aes(fill = LC_CPUE))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE (no.fish/angler hour) \nfrom Interview Data 2022")+
  #geom_sf(data = Only_ling_areas, color = "black", fill = "red")+
  coord_sf(xlim = c(LC_box[1] - 100000, LC_box[3] + 100000), 
           ylim = c(LC_box[2] - 100000, LC_box[4] + 100000))

###2022 CPUE using combined hours
int22[is.na(int22)] <- 0
int22$LC_CAUGHT <- int22$LCKEPT + int22$LCREL
int22$LC_CPUE_COMBI <- int22$LC_CAUGHT / int22$HOURS_COMBI

Ling_22_COMBI <- int22 %>% filter(HOURS_COMBI > 0) %>%  group_by(ADFGSTATLING) %>% 
  summarise(LC_CPUE_COMBI = mean(LC_CPUE_COMBI)) %>% filter(ADFGSTATLING > 0)
Ling_22_COMBI$STAT_AREA <- Ling_22_COMBI$ADFGSTATLING
Ling_22_COMBI_areas <- Ling_22_COMBI[c("STAT_AREA", "LC_CPUE_COMBI")]

Ling_plot_data_22_COMBI <- merge(stat_areas, Ling_22_COMBI_areas, by = "STAT_AREA")

LC_box <- st_bbox(Ling_plot_data_22_COMBI)


pdf("O:/DSF/GOAB/SASCODE/R Code/LC_CPUE_22.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data_22_COMBI, color = "black", aes(fill = LC_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE \n(no.fish/angler hour)\nfrom Interview Data 2022 \nusing combined hours")+
  geom_sf_text(data = Ling_plot_data_22_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(LC_box[1], LC_box[3]), 
           ylim = c(LC_box[2], LC_box[4]))
dev.off()

###2021 CPUE using combined hours
int21[is.na(int21)] <- 0
int21$LC_CAUGHT <- int21$LCKEPT + int21$LCREL
int21$HOURS_COMBI <- int21$HOURS_H + int21$HOURS_L + int21$HOURS_R + int21$HOURS_TOT
int21$LC_CPUE_COMBI <- int21$LC_CAUGHT / int21$HOURS_COMBI

Ling_21_COMBI <- int21 %>% filter(HOURS_COMBI > 0) %>%  group_by(ADFGSTATLING) %>% 
  summarise(LC_CPUE_COMBI = mean(LC_CPUE_COMBI)) %>% filter(ADFGSTATLING > 0)
Ling_21_COMBI$STAT_AREA <- Ling_21_COMBI$ADFGSTATLING
Ling_21_COMBI_areas <- Ling_21_COMBI[c("STAT_AREA", "LC_CPUE_COMBI")]

Ling_plot_data_21_COMBI <- merge(stat_areas, Ling_21_COMBI_areas, by = "STAT_AREA")

LC_box <- st_bbox(Ling_plot_data_21_COMBI)


pdf("O:/DSF/GOAB/SASCODE/R Code/LC_CPUE_21.pdf")
ggplot()+
  geom_sf(data = Alaska, color = "black", fill = "grey")+
  geom_sf(data = stat_areas, color = "black", fill = "transparent")+
  geom_sf(data = Ling_plot_data_21_COMBI, color = "black", aes(fill = LC_CPUE_COMBI))+
  scale_fill_distiller(palette = "Spectral", "Avg. Lingcod CPUE \n(no.fish/angler hour)\nfrom Interview Data 2021 \nusing combined hours")+
  geom_sf_text(data = Ling_plot_data_21_COMBI, aes(label = STAT_AREA), size = 3, check_overlap = TRUE, fontface = "bold")+
  coord_sf(xlim = c(LC_box[1], LC_box[3]), 
           ylim = c(LC_box[2], LC_box[4]))
dev.off()

