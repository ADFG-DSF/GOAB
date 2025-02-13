#######################################################
# Functions for use in GOAB files
# To pull these functions into a script, use source("functions.R")
#
# Created by CWM on 11/15/24
#######################################################


# Load in all .csv datasets from a designated file
get_data <- function(a) {
  print(a)
  
  files <- list.files(
    path = a,
    pattern = "*.csv",
    full.names = F,
    recursive = FALSE
  )
  
  
  for (i in seq(1, length(files))) {
    print(files[[i]])
    n <- gsub(".csv", "", files[[i]])
    assign(n, read.csv(paste0(a, files[[i]])), envir = .GlobalEnv)
    
  }
  
}

# Attribute data to Commercial Fisheries Management Units based on ADFG groudfish statistical areas.
  # This code is written for GOAB bio data, with the column STATAREA containing statistical areas
area_split_cfmu <- function(a) {
  a %>%
    mutate(
      CFMU = case_when(
        !is.na(STATAREA) &
          floor(STATAREA / 1000) %in% c(26) ~ "MAINLAND",
        # Mainland salmon stat areas, Area Q/R!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(27, 576) ~ "CHIGNIK",
        # Chignik salmon stat area, Area R!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(28) ~ "SAKPEN",
        # South AK Peninsula Eastern District salmon stat areas, Area R!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(30, 664, 674, 684, 694) ~ "ALEUTIAN",
        # Sotuh AK Peninsula Western District salmon stat areas, Area R!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(31) ~ "BERING",
        # Any harvest in Bering Sea, salmon stat areas!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(445, 446, 455, 465) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(496, 105) &
          !STATAREA %in% c(496100, 496030) ~ "NG",
        # North Gulf Coast, Area J, Stat Area in 2016 listed as 105932 but likely 505932
        STATAREA %in% c(496100, 496030) ~ "CI",
        # These areas have similar stat area to NG but are in CI!is.na(STATAREA) &
        floor(STATAREA / 1000) %in% c(221, 223, 516, 526) ~ "CI",
        # Cook Inlet, Area P!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25110 &
          STATAREA <= 25235 ~ "AFOGNAK",
        # Afognak salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25311 &
          STATAREA <= 25640 ~ "WESTSIDE",
        # Westside salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25710 &
          STATAREA <= 25770 ~ "SOUTHWEST",
        # Southwest salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25810 &
          STATAREA <= 25830 ~ "EASTSIDE",
        # Eastside salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25840 &
          STATAREA <= 25870 ~ "SOUTHEAST",
        # Southeast salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25880 &
          STATAREA <= 25890 ~ "SOUTHWEST",
        # Southwest salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25910 &
          STATAREA <= 25940 ~ "NORTHEAST",
        # Northeast salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 25 &
          STATAREA >= 25941 &
          STATAREA <= 25948 ~ "EASTSIDE",
        # Eastside salmon stat areas, Area Q!is.na(STATAREA) &
        floor(STATAREA / 1000) == 222 &
          STATAREA >= 222000 &
          STATAREA <= 222050 ~ "CI",
        # Cook Inlet, Area P!is.na(STATAREA) &
        floor(STATAREA / 1000) == 222 &
          STATAREA %in% c(222060) ~ "NG",
        # North Gulf Coast, Area P!is.na(STATAREA) &
        floor(STATAREA / 1000) == 456 &
          STATAREA >= 456001 &
          STATAREA <= 456004 ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 456 &
          STATAREA %in% c(456031, 456032) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 466 &
          STATAREA %in% c(466001, 466002, 466004, 466005) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 466 &
          STATAREA %in% c(466003, 466031, 466032, 466033, 466100) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 475 &
          (STATAREA >= 475500 &
             STATAREA <= 475932 |
             STATAREA %in% c(475934)) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 475 &
          STATAREA %in% c(475933) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 476 &
          STATAREA %in% c(476001, 476002) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 476 &
          (STATAREA >= 476003 &
             STATAREA <= 476102) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 485 &
          (
            STATAREA >= 485430 &
              STATAREA <= 485831 |
              STATAREA %in% c(485901, 485931, 485935)
          ) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 485 &
          (STATAREA %in% c(485832, 485902, 485933, 485934)) ~ "NG",
        # North Gulf Coast, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 485 &
          (STATAREA %in% c(485932)) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 486 &
          (STATAREA %in% c(486002)) ~ "PWSO",
        # PWS Outside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 486 &
          (STATAREA %in% c(486001) |
             (STATAREA >= 486003 &
                STATAREA <= 486100)) ~ "PWSI",
        # PWS Inside, Area J!is.na(STATAREA) &
        floor(STATAREA / 1000) == 495 ~ case_when(
          (STATAREA >= 495400 &
             STATAREA <= 495700) ~ "EASTSIDE",
          # Eastside, Area Q
          (STATAREA >= 495901 &
             STATAREA <= 495939 |
             STATAREA == 495831) ~ "NG",
          # North Gulf Coast, Area J
          (STATAREA == 495730) ~ "NORTHEAST",
          # Northeast, Area Q
          (STATAREA %in% c(495800, 495832)) ~ "AFOGNAK" # Afognak, Area Q
        ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 505 ~ case_when(
            (STATAREA >= 505330 & STATAREA <= 505700) ~ "EASTSIDE",
            (STATAREA >= 505901 &
               STATAREA <= 505934 | STATAREA == 505831) ~ "NG",
            (STATAREA == 505730) ~ "NORTHEAST",
            (STATAREA %in% c(505800, 505832)) ~ "AFOGNAK"
          ),!is.na(STATAREA) & floor(STATAREA / 1000) == 506 ~ "CI",!is.na(STATAREA) &
          floor(STATAREA / 1000) == 515 ~ case_when(
            (STATAREA >= 515300 & STATAREA <= 515700) ~ "EASTSIDE",
            (
              STATAREA >= 515901 &
                STATAREA <= 515906 | STATAREA %in% c(515831, 515832)
            ) ~ "NG",
            (STATAREA == 515730) ~ "NORTHEAST",
            (STATAREA %in% c(515801, 515802, 515833)) ~ "AFOGNAK",
            (STATAREA >= 515907 & STATAREA <= 515939) ~ "CI"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 525 ~ case_when(
            (STATAREA >= 525230 & STATAREA <= 525703) ~ "EASTSIDE",
            (STATAREA %in% c(525831, 525835, 525836, 525837)) ~ "NG",
            (STATAREA %in% c(525731, 525732, 525733)) ~ "NORTHEAST",
            (
              STATAREA >= 525801 &
                STATAREA <= 525807 |
                STATAREA %in% c(525832, 525833, 525834)
            ) ~ "AFOGNAK",
            (STATAREA >= 525901 & STATAREA <= 525932) ~ "CI"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 535 ~ case_when(
            (
              STATAREA >= 535230 &
                STATAREA <= 535634 |
                STATAREA %in% c(535703, 535704, 535705)
            ) ~ "SOUTHEAST",
            (STATAREA %in% c(535635, 535702)) ~ "SOUTHWEST",
            (STATAREA %in% c(535701, 535731, 535732, 535733, 535734)) ~ "WESTSIDE",
            (STATAREA %in% c(535706, 535707)) ~ "EASTSIDE",
            (STATAREA %in% c(535801, 535832)) ~ "MAINLAND",
            # Mainland, Area R
            (STATAREA %in% c(535802, 535803, 535831)) ~ "AFOGNAK",
            (STATAREA %in% c(535833, 535834)) ~ "NG",
            (STATAREA >= 535901 & STATAREA <= 535933) ~ "CI"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 545 ~ case_when(
            (STATAREA >= 545200 &
               STATAREA <= 545633 | STATAREA == 545704) ~ "SOUTHWEST",
            (STATAREA %in% c(
              545701, 545702, 545703, 545732, 545733, 545734
            )) ~ "WESTSIDE",
            (STATAREA %in% c(545731, 545801, 545802, 545803)) ~ "MAINLAND",
            (STATAREA == 545804) ~ "AFOGNAK",
            (STATAREA == 545900) ~ "CI"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 555 ~ case_when(
            (STATAREA >= 555200 & STATAREA <= 555630) ~ "SOUTHWEST",
            (STATAREA == 555701) ~ "WESTSIDE",
            (STATAREA %in% c(555702, 555731, 555732, 555733)) ~ "MAINLAND"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 565 ~ case_when(
            (STATAREA %in% c(
              565131, 565201, 565231, 565301, 565331, 565401
            )) ~ "SAKPEN",
            (
              STATAREA %in% c(
                565132,
                565202,
                565232,
                565302,
                565333,
                565403,
                565432,
                565502,
                565534,
                565604,
                565635
              )
            ) ~ "SOUTHWEST",
            (
              STATAREA %in% c(
                565332,
                565402,
                565431,
                565501,
                565531,
                565533,
                565602,
                565603,
                565631,
                565632,
                565633,
                565634,
                565701,
                565703
              )
            ) ~ "CHIGNIK",
            (STATAREA %in% c(565702, 565704)) ~ "MAINLAND"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 575 ~ case_when(
            (
              STATAREA >= 575131 &
                STATAREA <= 575401 | STATAREA %in% c(575404, 575431)
            ) ~ "SAKPEN",
            (STATAREA >= 575731 & STATAREA <= 575830) ~ "BERING",
            (
              STATAREA >= 575432 &
                STATAREA <= 575635 | STATAREA %in% c(575402, 575403)
            ) ~ "CHIGNIK"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 585 ~ case_when(
            (STATAREA >= 585100 &
               STATAREA <= 585431 | STATAREA == 585501) ~ "SAKPEN",
            (STATAREA >= 585631 & STATAREA <= 858830) ~ "BERING",
            (
              STATAREA %in% c(585432, 585502, 585531, 585532, 585601, 585602, 585603)
            ) ~ "CHIGNIK"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 595 ~ case_when(
            (STATAREA >= 595100 & STATAREA <= 595533) ~ "SAKPEN",
            (STATAREA >= 595631 & STATAREA <= 595833) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 605 ~ case_when(
            (STATAREA >= 605100 & STATAREA <= 605533) ~ "SAKPEN",
            (STATAREA >= 605534 & STATAREA <= 605834) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 615 ~ case_when(
            (STATAREA >= 615030 & STATAREA <= 615532) ~ "SAKPEN",
            (STATAREA >= 615533 & STATAREA <= 615834) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 625 ~ case_when(
            (STATAREA >= 625030 & STATAREA <= 625502) ~ "SAKPEN",
            (STATAREA >= 625503 & STATAREA <= 625832) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 635 ~ case_when(
            (STATAREA >= 635030 &
               STATAREA <= 635436 | STATAREA == 635502) ~ "SAKPEN",
            (STATAREA == 635501 |
               STATAREA >= 635503 & STATAREA <= 635800) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 645 ~ case_when(
            (
              STATAREA >= 545500 &
                STATAREA <= 645401 |
                STATAREA %in% c(645403, 645404, 645405, 645432)
            ) ~ "SAKPEN",
            (STATAREA == 645402 |
               STATAREA >= 645406 & STATAREA <= 645431) ~ "ALEUTIAN",
            (STATAREA >= 645433 & STATAREA <= 645730) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 655 ~ case_when(
            (STATAREA >= 655000 & STATAREA <= 655413) ~ "ALEUTIAN",
            (STATAREA >= 655430 & STATAREA <= 655700) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 665 ~ case_when(
            (STATAREA >= 665000 & STATAREA <= 665405) ~ "ALEUTIAN",
            (STATAREA >= 665430 & STATAREA <= 665600) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 675 ~ case_when(
            (STATAREA >= 675000 & STATAREA <= 675400) ~ "ALEUTIAN",
            (STATAREA >= 675430 & STATAREA <= 675600) ~ "BERING"
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 685 ~ case_when(
            (STATAREA >= 685000 & STATAREA <= 685400) ~ "ALEUTIAN",
            (STATAREA >= 685430 & STATAREA <= 685600) ~ "BERING",
          ),!is.na(STATAREA) &
          floor(STATAREA / 1000) == 695 ~ case_when(
            (STATAREA >= 695000 & STATAREA <= 695400)  ~ "ALEUTIAN",
            (STATAREA >= 695430 & STATAREA <= 695600) ~ "BERING"
          ),
        TRUE ~ 'ZZZZ'
      )
    )
  
}

# Attribute data to Sport Fish Management Areas based on ADFG groudfish statistical areas.
# This code is written for GOAB bio data, with the column STATAREA containing statistical areas
area_split_sf <- function(a) {
  a %>%
    mutate(
      SFmgmtarea = case_when(
        STATAREA >= 440000 &
          STATAREA < 480000 | STATAREA %in% c(
            485430,
            485500,
            485530,
            485600,
            485630,
            485700,
            485730,
            485800,
            485831,
            485901,
            485931,
            485932,
            485935,
            486001,
            486002,
            486003,
            486004,
            486005,
            486031,
            486032,
            486033,
            486034,
            486100
          ) ~ 'PWS',
        STATAREA %in% c(
          485832,
          485902,
          485933,
          485934,
          485935,
          486002,
          495831,
          495901,
          495902,
          495931,
          495932,
          495933,
          495934,
          495935,
          495936,
          495937,
          495938,
          495939,
          496001,
          496002,
          505831,
          505901,
          505902,
          505903,
          505904,
          505905,
          505906,
          505907,
          505908,
          505909,
          505931,
          505932,
          505933,
          505934
        ) ~ 'NG',
        STATAREA %in% c(
          495800,
          495832,
          505700,
          505730,
          505800,
          505832,
          515630,
          515700,
          515730,
          515801,
          515802,
          515833,
          525600,
          525630,
          525701,
          525702,
          525703,
          525731,
          525732,
          525733,
          525801,
          525802,
          525803,
          525804,
          525805,
          525806,
          525807,
          525832,
          525833,
          525834,
          535601,
          535602,
          535631,
          535632,
          535633,
          535634,
          535701,
          535702,
          535703,
          535704,
          535705,
          535706,
          535707,
          535731,
          535732,
          535733,
          535734,
          535802,
          535803,
          535831,
          545601,
          545602,
          545631,
          545632,
          545633,
          545701,
          545702,
          545703,
          545704,
          545732,
          545733,
          545734,
          545804,
          555630,
          555701,
          555733
        ) ~ 'KOD',
        STATAREA %in% c(555731, 555732, 545731, 545801, 545802, 545803, 535801, 535832) ~ 'AKPen',
        STATAREA %in% c(
          515831,
          515832,
          515901,
          515902,
          515903,
          515904,
          515905,
          515906,
          515907,
          515908,
          515931,
          515932,
          515933,
          515934,
          515935,
          515936,
          515937,
          515938,
          515939,
          516001,
          516002,
          525831,
          525835,
          525836,
          525837,
          525901,
          525902,
          525931,
          525932,
          526002,
          526003,
          535833,
          535834,
          535901,
          535902,
          535903,
          535904,
          535905,
          535906,
          535931,
          535932,
          535933,
          545900
        ) ~ 'CI',
        TRUE ~ 'ZZZ'
      ),
      SFmgmtarea = case_when(
        is.na(SFmgmtarea) | SFmgmtarea == 'ZZZ' ~ case_when(
          PORT %in% c('Homer', 'CCI') ~ 'CI',
          PORT == 'Kodiak' ~ 'KOD',
          PORT %in% c('Whittier', 'Valdez', 'Cordova') ~ 'PWS',
          PORT == 'Seward' ~ 'NG',
          TRUE ~ PORT
        ),
        TRUE ~ SFmgmtarea
      )
    )
}

# Standard Error calculation
stderr <- function(values){ # Standard Error calculation
  sd(values, na.rm = TRUE) / sqrt(length(values))
}

# Find mode of a variable. Used mainly for rockfish common year classes
mode <- function(x) {
  x <- x[!is.na(x)]  # Remove NAs
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}