Co-authors: Kennedy Lushasi, Sarah Hayes, Katie Hampson

Data and scripts for "Reservoir Dynamics of Rabies in Southeast Tanzania and the roles of cross-species transmission and domestic dog vaccination”.
Identifying features such as names and GPS coordinates have been removed. 
Simulated coordinates would therefore be needed for some of the codes to run. 

The data files are described below:
1.	animal_bites: a list of biting animals with their unique identifier identified across the study areas.
2.	animal_cases_map: a list of animal rabies cases and their geographical coordinates indication the location of where they were identified.
3.	Case_numbers_all_areas: contains animal rabies cases from all areas across the study including northern Tanzania (Ngorongoro and Serengeti) and for Pemba Island
4.  Cluster_assignment: details of cases assigned to individual clusters
5.	Combined_animal_cases_ts: monthly cases of probable animal rabies 
6.	Combined_human_deaths_ts: monthly probable human deaths according to the species of animal that caused the exposure
7.	Combined_human_Exposure_ts: monthly probable human rabies exposures according to the species of animal that caused the exposure
8.	Dog_population_year: annual dog population estimate per ward
9.	human_bites:  human bite data traced over the study period
10.	human_exposures_prop: proportion of probable human rabies exposures by biting animal species
11. NBS2012_MasterVillagePop_shpmatched.csv: census data reporting the population by village
12. Number_of_dogs24: Estimates of dog numbers
13. seren_dists: Distances between known cases from Serengeti. Used for parameter estimation
14. seren_si: Serial intervals of known cases from Serengeti. Used for parameter estimation
15.	STzCellData_4kmsq: 4kmsq grid cells of the study area with unique identifying number for each cell
16.	STzdog_pop_matrix: Dog population data matrix for the study site
17.	STzHumanPopMat_CellByMonth_2010-01-01_to_2020-12-31: Human population matrix for the study site in southern Tanzania from 2010 to 2020
18.	Vacc_coverage_summary_STzdists: Summary statistics of vaccination coverage by district across the study site
19.	VaccinationRounds: Shows annual intervals of vaccination campaigns
20.	Vaccinated_cleaned: Contains number of dogs vaccinated in each round of vaccination campaign in study districts
21.	Transects_cleaned: Dog vaccination transect data
22.	NBS2012_VillagePop_matched: Human population data for the study districts
23.	human_pop_growth: Estimated human population growth rate in the study districts
24.	EstimatedDogs2015: Contains estimated dog population and vaccination coverage of the study districts


To run '6c.create_human_pop_Ngoro' and '6d.create_human_pop_Seren' the file 'data/tza_ppp_2012.tif' is called. This is not included due to size but can be downloaded from the WorldPop website (https://www.worldpop.org/)

Figures that are generated from the scripts and data are stored in subfolder: *figs*                                                                        
Output files generated from the scripts and data are stored in subfolder: *output*
Not all outputs are provided due to their size, but they can be created by running the scripts. 

The *R* subfolder contains helper functions for the scripts


