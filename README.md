
# Gulf of Alaska Bottomfish

**Author:** Clay McKean (<clay.mckean@alaska.gov>)  
  
**Last updated:** November 2024  
  
**Local Directory**  
There are 2 local directories associated with this project:  
**Homer:** Main directory O:/DSF/GOAB/  
Locally, this repository is housed at O:/DSF/GOAB/Git/  
**Anchorage** Clay’s S:/DSFShared/Groundfish Port Sampling program/  
  

## About this project

This project assesses the halibut, rockfish, and lingcod sport fisheries
in Southcentral Alaska by collecting biological samples and conducting
angler interviews out of the ports of Homer, Seward, Whittier, Valdez,
and Kodiak.  
The operational plan for this project can be found here:
<https://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.2A.2022.24.pdf>  

## Project Objectives

  

### Primary Objectives

1)  Estimate the mean weight of Pacific halibut taken by each user in
    each area of Southcentral Alaska (Kodiak, Lower Cook Inlet, Central
    Cook Inlet, North Gulf Coast, Eastern Prince William Sound, and
    Western Prince William Sound), such that the mean weight estimates
    for each user group in each area are within 0.20 of the true mean
    weight at least 90% of the time.  
2)  Estimate the length composition of the Pacific halibut harvest by
    area such that the estimated proportions are within 0.20 of the true
    proportions at least 95% of the time.  
3)  Estimate the species composition by port of the rockfish harvest
    landed at Kodiak, Homer, Seward, Whittier, and Valdez during May
    through September such that the estimated proportions of each
    species are within 0.20 of the true proportions at least 95% of the
    time.  
4)  Estimate the age, length, and sex composition by port of the
    principal rockfishes landed at Kodiak, Homer, Seward, Whittier, and
    Valdez during May through September such that the estimated
    proportions are within 0.20 of the true proportions at least 95% of
    the time.  
5)  Estimate the age, length, and sex composition by port of the lingcod
    harvest landed at Kodiak, Homer, Seward, Whittier, and Valdez during
    July through September such that the estimated proportions are
    within 0.20 of the true proportions at least 90% of the time.

### Secondary Objectives

1)  Identify differences in the geographic distribution of groundfish
    effort and harvest between user groups and across years for each
    port during May through September.  
2)  Estimate the proportion of the Pacific halibut harvest that was
    cleaned (and carcasses discarded) at sea at each port. These
    estimates will be used to stratify length and weight estimates at
    ports where cleaning at sea is prevalent.  
3)  Estimate the proportions of released Pacific halibut that were
    caught on circle hooks versus other types of hooks at each port.
    This information is needed to refine estimates of halibut release
    mortality in the sport fishery.  
4)  To refine discard mortality estimates, gather data on the depths of
    capture for pelagic and nonpelagic rockfish that were released.  
5)  Estimate the proportions of released lingcod that were of sublegal
    (under 35 inches total length) and legal size (35 inches and
    greater) for ports with a minimum size limit regulation. These data
    will provide information on future recruitment and abundance indices
    used for future stock assessments.  
6)  Biological data will be collected from salmon sharks (Lamna
    ditropis), Pacific sleeper sharks (Somniosus pacificus), and spiny
    dogfish (Squalus acanthias) harvested in the sport fishery in order
    to estimate the age, length, sex composition, and spatial
    distribution of the harvest. No sampling objectives are established
    for sharks because harvests are too small to generate reliable
    estimates for any given year. It is expected that age, length, and
    sex data will be compiled across a number of years and combined with
    commercial harvest sampling and other research programs to estimate
    life history parameters.

## Repository Directory

- 1.`data/`: Data folder  
  - 1.  `HAL/`: Halibut AWL data. Data is compiled each year from port
        sampling biological samples  

  - 2.  `Harvest/`: Rockfish and Lingcod harvest data, calculated by
        combining SWHS and logbook estimates  

    - 1.  `LC/`: Lingcod harvest data by port and sport fish management
          area  
    - 2.  `RF/`: Rockfish harvest data by port, sport fish management
          area, and species  

  - 3.  `Intervw/`: Port sampling interview data. Data is compiled each
        year from interview samples  

  - 4.  `LC/`: Lingcod AWL data. Data is compiled each year from port
        sampling biological samples  

  - 22. `RF/`: Rockfish AWL data. Data is compiled each year from port
        sampling biological samples  
- 2.`Figures/`: Output figures  
    
- 3.`reports/`: Reports associated with this repository  
  - 1.  Currently empty
- 4.`scripts/`: Scripts for running analyses  
  - 1.  `Age_comps/`: Age composition data for Rockfish and Lingcod  

    - 1.  `LC Age Comp bubble.R`: Lingcod age composition bubble plot  
    - 2.  `LCAgeComp9622_port.R`: Lingcod age composition by port  
    - 3.  `LCAgeComp9622_SFmgmtarea.R`: Lingcod age composition by sport
          fish management area  
    - 4.  `RF Age Comp bubble.R`: Rockfish age composition bubble plot  
    - 2.  `RFAgeComp9622_port.R`: Rockfish age composition by port  
    - 3.  `RFAgeComp9622_SFmgmtarea.R`: Rockfish age composition by
          sport fish management area  

  - 2.  `Apportionment/`: Apportioning groundfish to sport fish
        management areas to assess proportion of harvest by port and
        management area  

    - 1.  `Apportion halibut to mgmtareas.R`: Halibut apportionment  
    - 2.  `Apportion lingcod to mgmtareas.R`: Lingcod apportionment  
    - 3.  `Apportion rockfish to mgmtareas.R`: Rockfish apportionment  

  - 3.  `L.WRegression/`: Calculate yield of groundfish in kg, using
        length-weight regressions to estimate weights of fish when no
        weight is available in the data.  Computation is based on the
        basic formula: Yield(s) = Harvest(all s) \* SpeciesComp(s) \*
        MeanWt(s).  
        Estimates are preliminary because they are based on L-W
        regressions that haven’t yet been tested for differences between
        years, ports, sexes, etc.  

    - 1.  `LCYieldPrelim_SFmgmtarea.R`: Estimate of lingcod yield by
          sport fish management area by year  
    - 2.  `RFYieldPrelim_byAssemb_port.R`: Estimate of rockfish yield by
          port and assemblage by year  
    - 3.  `RFYieldPrelim_byAssemb_SFmgmtarea.R`: Estimate of rockfish
          yield by sport fish management area and assemblage by year  

  - 4.  `Spatial harvest and effort/` Total harvest by port, user, and
        year from port sampling interview data. Used for providing
        harvest trends in managemnet reports/

    - 1.  `LC/`: Lingcod data/

      - (i). `LC_report_spatial_subset.R`: Lingcod interview harvest
        data for lingcod report/

    - 2.  `RF/`: Rockfish data. currently empty/

  - 22. `Species_comp/`: Rockfish species composition data from port
        sampling biological sampling/

    - 1.  `RF by assemblage.R`: Species compositon and average weights
          per year at the assemblage level. Predicted weights based on
          length used when weight is not available for a sample/
    - 2.  `RF_simple_spcomp.R`: Simple species composition by sport fish
          management area. Useful for providing species comps to area
          managers  
          -c. `RFspcomp9622.R`: Rockfish species composition in
          biological data. Uses Steve Fleischman version of composition
          equations  

  - 6.  `AWLsummaries.R`: Summary statistics of rockfish data. For
        rockfish report  

  - 7.  `FractionCleanedAtSea.R`: Calculates the fraction of harvest
        cleaned at sea by port for halibut, rockfish, and lingcod. Used
        in writing Op plan.  
- 5.  `functions.R`: Functions used in R scripts. Accessed with
      `source("functions.R")`  
