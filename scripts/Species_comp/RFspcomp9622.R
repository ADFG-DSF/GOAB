##########################################################################
# RFspcomp9616_awl.sas - rockfish SPECIES composition based on AWL samples
# CWM 06/09/23
# Uses Steve Fleischman version of composition equations
# 
# #########################################################################
#   1. Insufficient rockfish data to estimate SPECIES comp or other objectives
# for CCI. Total rockfish sample size for 1996-2016 was only 86 fish, 83 of
# which were sampled 2010-2016. Instead, CCI and Homer data pooled for 
# estimates for Cook Inlet.  
# 2. Don't bother with estimate of SPECIES comp for Whittier 1998 - only sampled
# 			for one month, sample sizes too small, estimates likely to be biased.
# 	3. At most ports through 2003, we didn't make a distinction between dusky and
# dark rf. Separated dusky/dark at Kodiak in 2003, and at all remaining
# ports starting in 2004. However, there were still some fish from 2004 on
# that were either dusky or dark (couldn't tell), and they were
# 			coded as either 169 (unspecified pelagic) or 154 (dusky/dark). SPECIES comp
# 			estimates include the dusky/dark (154) so that proportions for all other 
# 			SPECIES are not inflated. 		
# ########################################################################

# To do: After getting estimates of SPECIES comp from AWL data, reload the data
# including unspecified pelagic, demersal, and slope, and compare the 
# estimates of assemblage composition to estimates of assemblage 
# composition from interview data.
#
# Translated from SAS to R by CWM on 06/09/23
##########################################################################



library(tidyverse)
library(readxl)
library(survminer)

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
detach(package:plyr)


spcomp_init <- rf %>% 
  filter(YEAR >= 1996) %>% 
  mutate(
    abbrev = case_when(
      SP == 142 ~ "BLK",
      SP == 172 ~ "DUS",
      SP == 173 ~ "DRK",
      SP == 154 ~ "DUSDRK",
      SP == 155 ~ "YTAL",
      SP == 156 ~ "WID",
      SP == 177 ~ "BGL",
      SP == 137 ~ "BOC",
      SP == 146 ~ "CAN",
      SP == 149 ~ "CHI",
      SP == 138 ~ "COP",
      SP == 176 ~ "HAR",
      SP == 136 ~ "NOR",
      SP == 147 ~ "QUI",
      SP == 158 ~ "RSTP",
      SP == 150 ~ "ROS",
      SP == 151 ~ "REYE",
      SP == 152 ~ "SRAK",
      SP == 166 ~ "SHCN",
      SP == 157 ~ "SGRY",
      SP == 148 ~ "TIG",
      SP == 184 ~ "VER",
      SP == 145 ~ "YE"
    ),
    USER = case_when(
      USER == "" ~ "Unknown",
      TRUE ~ USER
    ),
    PORT = case_when(
      PORT %in% c("Homer", "CCI") ~ "CI",
      TRUE ~ PORT
    )
  )

# create new data set
spcomp.rfcompdata <- spcomp_init

# filter data
spcomp_init$RARE <- ifelse(spcomp_init$YEAR >= 2000, "NR", spcomp_init$RARE)
spcomp <- spcomp_init[spcomp_init$RARE != "R", ]

# count frequency of each SPECIES by port, user, year
comp <- spcomp %>% group_by(PORT, USER, YEAR, SPECIES) %>% summarise(count = n())

# restructure data file
comp2 <- comp %>% 
  group_by(PORT, YEAR, SPECIES) %>% 
  summarise(niC = sum(ifelse(USER == "Charter", count, 0)),
            niP = sum(ifelse(USER == "Private", count, 0)),
            niU = sum(ifelse(USER == "Unknown", count, 0)), 
            niM = sum(ifelse(USER == "SewMilC", count, 0)))

#Back to GPT - think this needs to be redone
comp2_2 <- comp2 %>% 
  mutate(
    SPECIES = case_when(
      SPECIES == "Blackgll" ~ "Blackgill",
      SPECIES == "Quill" ~ "Quillback",
      SPECIES == "Redstrpe" ~ "Redstripe",
      SPECIES == "Harleq" ~ "Harlequin",
      SPECIES == "Rosethrn" ~ "Rosethorn",
      SPECIES == "Shortrkr" ~ "Shortraker",
      SPECIES == "Shrpchin" ~ "Sharpchin",
      SPECIES == "Silvergr" ~ "Silvergray",
      SPECIES == "Vermilon" ~ "Vermilion",
      SPECIES == "Yelleye" ~ "Yelloweye",
      SPECIES == "Yelltail" ~ "Yellowtail",
      SPECIES == "DrkDsky" ~ "DuskyDark" 
    )
  )

totaln <- comp2_2 %>% filter(niP > 0) %>% group_by(PORT, YEAR) %>%  
  summarise(nC = sum(niC),nP = sum(niP),nU = sum(niU),nM = sum(niM))


comp3 <- merge(comp2_2, totaln, by = c("PORT", "YEAR"))
comp3$n <- with(comp3, niC + niP + niU + niM)
comp3$piC <- with(comp3, niC/nC)
comp3$vpiC <- with(comp3, piC*(1-piC)/(nC-1))
comp3$piP <- with(comp3, niP/nP)
comp3$vpiP <- with(comp3, piP*(1-piP)/(nP-1))
comp3$piU <- with(comp3, niU/nU)
comp3$vpiU <- with(comp3, piU*(1-piU)/(nU-1))
comp3$piU <- NULL
comp3$vpiU <- NULL

pcheck <- aggregate(cbind(piC, piP) ~ PORT + YEAR, data = comp3, sum)
pcheck$sumC <- NULL
pcheck$sumP <- NULL
#Loading excel data

pharv <- read_xlsx("data/Harvest/RF/RFHarvByUser9619.xlsx", sheet = "Sheet3")
pharv <- pharv[1:8]


pharv <- pharv %>% rename("PORT" = "Port",
                          "YEAR" = "Year",
                          "HC" = "Charv",
                          "SEHC" = "SECharv",
                          "HP" = "Pharv",
                          "SEHP" = "SEPharv",
                          "H" = "Harv",
                          "SEH" = "SEHarv")

pharv$vHC <- with(pharv, SEHC^2)
pharv$vHP <- with(pharv, SEHP^2)
pharv$vH <- with(pharv, SEH^2)
pharv$SEHC <- NULL
pharv$SEHP <- NULL
pharv$SEH <- NULL


comp4 <- merge(comp3, pharv, by=c("PORT", "YEAR"))

comp4$ni <- comp4$niC + comp4$niP + comp4$niU + comp4$niM
comp4$pi2 <- comp4$ni / comp4$n



comp4 <- within(comp4, {
  HiC <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), piC * HC, NA)
  vHiC <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), piC^2 * vHC + vpiC * HC^2 - vpiC * vHC, NA)
  SEHiC <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), sqrt(vHiC), NA)
  HiP <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), piP * HP, NA)
  vHiP <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), piP^2 * vHP + vpiP * HP^2 - vpiP * vHP, NA)
  SEHiP <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), sqrt(vHiP), NA)
  Hi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), HiC + HiP, NA)
  vHi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), vHiC + vHiP, NA)
  SEHi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), sqrt(vHiC + vHiP), NA)
  pi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), Hi / H, NA)
  vpi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), 
                (1/H^2) * (vHC * (piC * HP - HiP)^2 / H^2 + vHP * (piP * HC - HiC)^2 / H^2 + vpiC * HC^2 + vpiP * HP^2), NA)
  SEpi <- ifelse(PORT != "NG" | (PORT == "NG" & YEAR >= 2001), sqrt(vpi), NA)
})   
######
comp4 <- subset(comp4, !(PORT == "Whittier" & YEAR >= 1996 & YEAR <= 1998)) 
######
comp4 <- subset(comp4, select = -c(vpi))

#print(comp4, digits = 10) #commented out, useful as data check

comp4 <- comp4 %>% filter(YEAR >= 1996) %>%
  group_by(PORT, YEAR, SPECIES, n, piC, vpiC, HiC, SEHiC, piP, vpiP, 
           HiP, SEHiP, pi, SEpi, Hi, SEHi)
#bubble plots for publication, trying to make bubble plot for main RF SPECIES
# filter data
plotcomp <- comp4 %>%
  filter(PORT == "Whittier",
         YEAR <= 2016,
         YEAR >= 1996,
         SPECIES %in% c("Black", "Dark", "Dusky", "Yelloweye", "Copper", "China", "Quillback", "Tiger")) %>%
  mutate(rank = case_when(SPECIES == "Black" ~ 1,
                          SPECIES == "Dark" ~ 2,
                          SPECIES == "Dusky" ~ 3,
                          SPECIES == "Yelloweye" ~ 4,
                          SPECIES == "Copper" ~ 5,
                          SPECIES == "China" ~ 6,
                          SPECIES == "Quillback" ~ 7,
                          SPECIES == "Tiger" ~ 8)) %>%
  arrange(PORT, YEAR, rank)

ggplot(data = plotcomp, aes(x = YEAR, y = SPECIES, size = pi))+
  geom_point(shape = 21, alpha = 0.5, stroke = 0.1, fill = "lightgray")


# create bubble plot

ggplot(plotcomp, aes(x = YEAR, y = SPECIES, size = pi)) +
  geom_point(shape = 21, alpha = 0.5, stroke = 0.1) +
  scale_fill_manual(values = "lightgray") +
  scale_size(range = c(0.3, 3), guide = "none") +
  theme_bw() +
  xlab("YEAR") +
  ylab("SPECIES") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "gray", size = 0.1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# create appendix table of SPECIES proportions by port and year

comp4_Tpi <- comp4 %>% group_by(PORT, YEAR, SPECIES) %>% summarise(proportion = pi)



print(comp4_Tpi, n = nrow(comp4_Tpi))

# create appendix table of standard errors of SPECIES proPORTions by PORT and YEAR

comp4_TSEpi <- comp4 %>% group_by(PORT, YEAR, SPECIES) %>% summarise(SE_proportion = SEpi)


print(comp4_TSEpi, n = nrow(comp4_TSEpi))

#Looking at "Other" RF SPECIES
comp5 <- comp4


otherspp <- !(comp4$SPECIES %in% c("Black","Yelloweye","Quillback","Copper",
                                   "Dusky","Dark","Duskydrk"))
comp5$SPECIES[otherspp] <- "Other"

comp5_TSpi <- comp5 %>% group_by(PORT, YEAR, SPECIES) %>% summarise(proportion = pi)



# Plot using ggplot2

comp4blackYE <- comp4 %>% filter(SPECIES %in% c("Black", "Yelloweye"))

  ggplot(data = comp4blackYE, aes(x = YEAR, y = pi, group = SPECIES, color = SPECIES)) +
  geom_line() +
  facet_wrap(~ PORT, nrow = 5, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Harvest by SPECIES 1996-current", color = "SPECIES") +
  theme_bw() +
  theme(panel.spacing = unit(10, "pt"))

# Transpose to get harvest of each SPECIES by port and year
comp4_THi <- comp4[c("PORT", "YEAR", "SPECIES", "Hi")] 


# Print table
cat("Harvest by SPECIES 1996-current\n")
print(comp4_THi)


comp4_THi_plot <- comp4_THi
otherspp <- !(comp4_THi$SPECIES %in% c("Black","Yelloweye",
                                     "Dusky","Dark","Duskydark"))
comp4_THi_plot$SPECIES[otherspp] <- "Other"  


  ggplot(data = comp4_THi_plot, aes(x = YEAR, y = Hi, fill = SPECIES)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ PORT, nrow = 5, scales = "free_y") +
  scale_fill_manual(values = c("Black" = "black", "Dusky" = "lightgrey", 
                               "Dark" = "darkgrey", "Yelloweye" = "yellow", "Other" = "ivory")) +
  labs(title = "Harvest by SPECIES 1996-2015", y = "Harvest (lbs)", fill = "SPECIES") +
  theme_bw() +
  theme(panel.spacing = unit(10, "pt"))

  # Plot proportion in harvest
  ggplot(data=comp4_THi_plot, aes(x=YEAR, y=pi, fill=SPECIES)) +
    geom_col(position=position_stack(reverse=TRUE), color="black", size=0.2) +
    facet_grid(PORT ~ ., scales = "free_y", space="free_y") +
    scale_fill_manual(values = c("Black" = "black", "Dusky" = "lightgrey", 
                                 "Dark" = "darkgrey", "Yelloweye" = "yellow", "Other" = "ivory")) +
    labs(x="Year", y="Proportion in Harvest") +
    theme(panel.spacing.y = unit(0.1, "lines")) +
    theme(strip.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5))
  
  # Plot harvest (No. Fish)
  ggplot(data=comp4_THi_plot, aes(x=YEAR, y=Hi, fill=SPECIES)) +
    geom_col(position=position_stack(reverse=TRUE), color="black", size=0.2) +
    facet_grid(PORT ~ ., scales = "free_y", space="free_y") +
    scale_fill_manual(values = c("Black" = "black", "Dusky" = "lightgrey", 
                                 "Dark" = "darkgrey", "Yelloweye" = "yellow", "Other" = "ivory")) +
    labs(x="Year", y="Harvest (No. Fish)") +
    theme(panel.spacing.y = unit(0.1, "lines")) +
    theme(strip.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5))
  

  
  

#Get the number of SPECIES
  numspp <- comp4 %>% group_by(PORT, YEAR) %>% summarise(NumSPECIES = n_distinct(SPECIES))

#Now get the total sample sizes
  sampsize <- comp4 %>%
    group_by(PORT, YEAR) %>%
    summarise(n = sum(niC, niP, niU, niM)) %>%
    ungroup()

#Merge with number of SPECIES
  sampsize <- merge(sampsize, numspp, by = c("PORT", "YEAR"))
  
#Plot number of SPECIES vs. sample size
  ggplot(sampsize, aes(x = n, y = NumSPECIES)) +
    geom_point() +
    facet_grid(rows = vars(PORT), scales = "free_y") +
    labs(title = "Number of SPECIES vs. sample size (n)")
  
#Run numbers to determine the "primary" SPECIES for age/length/sex composition
  primarysp <- comp4 %>%
    group_by(PORT, SPECIES) %>%
    summarise(meanpi = mean(pi)) %>%
    ungroup()  

  #Plot distribution of estimated SPECIES proportions

  #2 ways to go here, all SPECIES and make a new plot per port, or one plot with an "Other" group
  
  #comp4_viol$SPECIES[otherspp] <- "Other"  
  
  c4w <- comp4 %>% filter(PORT == "Whittier")
  ggplot(data = c4w, aes(x = SPECIES, y = pi)) +
    geom_violin(trim = FALSE) +
    facet_grid(cols = vars(PORT), scales = "free_x") +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Distribution of estimated SPECIES proportions, 1996-current") +
    theme(legend.position = "none")
  
  comp4_viol <- comp4
  otherspp <- !(comp4_viol$SPECIES %in% c("Black","Yelloweye",
                                          "Dusky","Dark","Duskydark"))
  ggplot(data = comp4_viol, aes(x = SPECIES, y = pi)) +
    geom_violin(trim = FALSE) +
    facet_grid(cols = vars(PORT), scales = "free_x") +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Distribution of estimated SPECIES proportions, 1996-current") +
    theme(legend.position = "none")

  #Convert data to assemblage composition
  assemb <- comp4 %>%
    mutate(Assemblage = ifelse(SPECIES %in% c("Black", "Dark", "Dusky", "DuskyDark", "Yellowtail", "Widow"), "Pelagic", "Nonpelagic")) %>%
    select(PORT, YEAR, Assemblage, Hi, HiC, HiP, pi)
  #Summarize assemblage composition
  AssembComp <- assemb %>%
    group_by(PORT, Assemblage, YEAR) %>%
    summarise(Hasmb = sum(Hi), HC = sum(HiC), HP = sum(HiP), pAsmb = sum(pi)) %>%
    ungroup()
#Plot assemblage composition
    # Define a data frame to map attributes to bar colors
    barcolor <- data.frame(
      id = c("Assemb", "Assemb"),
      value = c("Pelagic", "Nonpelagic"),
      linecolor = c("black", "black"),
      fillcolor = c("black", "lightgray")
    )
    
    # Define the plot theme
    theme_set(theme_bw())
    
    # Create the panel plot
    ggplot(data = AssembComp, aes(x = YEAR, y = Hasmb)) +
      geom_col(aes(fill = Assemblage), position = "stack") +
      facet_wrap(vars(PORT), nrow = 5, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("Pelagic" = "black", "Nonpelagic" = "lightgray")) +
      scale_y_continuous(label = scales::comma) +
      labs(x = "Year", y = "Harvest (No. Fish)", fill = "") +
      theme(panel.spacing = unit(10, "pt"), strip.text = element_text(size = 12, face = "bold"))
    
    