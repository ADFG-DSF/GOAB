
# Gulf of Alaska Bottomfish

**Author:** Clay McKean (<clay.mckean@alaska.gov>)  
  
**Last updated:** February 2025  
  
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
  - i.`HAL/`: Halibut AWL data. Data is compiled each year from port
    sampling biological samples  
  - ii.`Harvest/`: Rockfish and Lingcod harvest data, calculated by
    combining SWHS and logbook estimates  
    - a.`LC/`: Lingcod harvest data by port and sport fish management
      area  
    - b.`RF/`: Rockfish harvest data by port, sport fish management
      area, and species  
  - iii.`Intervw/`: Port sampling interview data. Data is compiled each
    year from interview samples  
  - iv.`LC/`: Lingcod AWL data. Data is compiled each year from port
    sampling biological samples  
  - v.`RF/`: Rockfish AWL data. Data is compiled each year from port
    sampling biological samples  
  - vi.`RF_Harvest_Reconstruction/`: Statewide rockfish harvest and
    release estimates using the Howard et al. method. This data is used
    for the annual rockfish harvest reconstruction. 
    - This data comes from Phil Joy, and can be found at
      <https://github.com/ADFG-DSF/RockfishSportMort/output/>  
- 2.`Figures/`: Output figures  
    
- 3.`reports/`: Reports associated with this repository  
  - i.`RF_Harvest_Reconstruction.R`: This file contains the output file
    of the rockfish harvest reconstruction, titled
    `RF_Removals_thruXX.xlsx`. This file is sent out to the members of
    the Statewide Rockfish Initiative in early October, once the harvest
    and release estimates are recieved from Phil Joy.  
- 4.`scripts/`: Scripts for running analyses  
  - i.`Age_comps/`: Age composition data for Rockfish and Lingcod  
    - a.`LC Age Comp bubble.R`: Lingcod age composition bubble plot  
    - b.`LCAgeComp9622_port.R`: Lingcod age composition by port  
    - c.`LCAgeComp9622_SFmgmtarea.R`: Lingcod age composition by sport
      fish management area  
    - d.`RF Age Comp bubble.R`: Rockfish age composition bubble plot  
    - b.`RFAgeComp9622_port.R`: Rockfish age composition by port  
    - c.`RFAgeComp9622_SFmgmtarea.R`: Rockfish age composition by sport
      fish management area  

  - ii.`Apportionment/`: Apportioning groundfish to sport fish
    management areas to assess proportion of harvest by port and
    management area  
    - a.`Apportion halibut to mgmtareas.R`: Halibut apportionment  
    - b.`Apportion lingcod to mgmtareas.R`: Lingcod apportionment  
    - c.`Apportion rockfish to mgmtareas.R`: Rockfish apportionment  

  - iii.`L.WRegression/`: Calculate yield of groundfish in kg, using
    length-weight regressions to estimate weights of fish when no weight
    is available in the data.  Computation is based on the basic
    formula: Yield(s) = Harvest(all s) \* SpeciesComp(s) \* MeanWt(s).  
    Estimates are preliminary because they are based on L-W regressions
    that haven’t yet been tested for differences between years, ports,
    sexes, etc.  
    - a.`LCYieldPrelim_SFmgmtarea.R`: Estimate of lingcod yield by sport
      fish management area by year  
    - b.`RFYieldPrelim_byAssemb_port.R`: Estimate of rockfish yield by
      port and assemblage by year  
    - c.`RFYieldPrelim_byAssemb_SFmgmtarea.R`: Estimate of rockfish
      yield by sport fish management area and assemblage by year  

  - 4.  `Objectives`: Analyzing primary and secondary objectives of the
        port sampling program  

    - 1.  `Objectives 1 & 2`: Estimate mean weight and length of Pacific
          halibut by user group and sport fish area  
    - 2.  `Objective 3`: Estimate species compe of rockfish by port  
    - 3.  `Objective 4`: Estimate age, length, and sex composition of
          rockfish by port for principal rockfish species
          (black/yelloweye)  
    - 4.  `Objective 5`: EStimate age, length, and sex composition of
          lingcod by port  
    - 5.  `Secondary Objective 1`: Identify differences in geographic
          distribution of groundfish harvest and effort by user group
          and year  
    - 6.  `Secondary Objective 2`: Estimate proportion of Pacific
          halibut harvest cleaned at sea in each port  
    - 7.  `Secondary Objective 3`: Estimate proportion of Pacific
          halibut caught on circle hooks vs. other hook types at each
          port  
    - 8.  `Secondary Objective 4`: Evaluated data on discarded rockfish
          depth of capture for pelagic, yelloweye, and non-pelagic
          assemblages  
    - 1.  `Secondary Objective 5`: Estimate proportions of released
          lingcod of sublegal size by port  

  - v.`RF_Reconstruction`: Rockfish harvest reconstruction using harvest
    and release estimates from RTS.  
    - a.`RF_removals`: Estimates the total removals in lbs. for Black
      and Yelloweye rockfish using harvest and release estimates from
      Phil Joy, using the Howard et al. method. This script should be
      run in early October after recieving the updated harvest and
      release estimates. Be sure to update the script to add the port
      sampling data for the current year and update the ouput file to
      state the current year.  

  - vi.`Spatial harvest and effort/`: Total harvest by port, user, and
    year from port sampling interview data. Used for providing harvest
    trends in managemnet reports/
    - a.`LC/`: Lingcod data/
      - (i).`LC_report_spatial_subset.R`: Lingcod interview harvest data
        for lingcod report/
    - b.`RF/`: Rockfish data. currently empty/

  - vii.`Species_comp/`: Rockfish species composition data from port
    sampling biological sampling/
    - a.`RF by assemblage.R`: Species compositon and average weights per
      year at the assemblage level. Predicted weights based on length
      used when weight is not available for a sample/
    - b.`RF_simple_spcomp.R`: Simple species composition by sport fish
      management area. Useful for providing species comps to area
      managers  
    - c.`RFspcomp9622.R`: Rockfish species composition in biological
      data. Uses Steve Fleischman version of composition equations  

  - viii.`AWLsummaries.R`: Summary statistics of rockfish data. For
    rockfish report  

  - viv.`FractionCleanedAtSea.R`: Calculates the fraction of harvest
    cleaned at sea by port for halibut, rockfish, and lingcod. Used in
    writing Op plan.  
- 5.`functions.R`: Functions used in R scripts. Accessed with
  `source("functions.R")`  
