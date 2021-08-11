GENERAL INFORMATION
1. Title of Dataset: Reservoir Dynamics of Rabies in Southeast Tanzania and the roles of cross-species transmission and domestic dog vaccination
2. Author Information
	A.Investigator Contact Information 
	
             Name: Kennedy Lushasi
	     Institution: Ifakara Health Institute, Dar es salaam-Tanzania
	     Email: klushasi@ihi.or.tz

	B.  Investigator Contact Information
		Name: Sarah Hayes
		Institution:  Imperial College, London-UK
		Email: sarah.hayes16@imperial.ac.uk

	C. Alternate Contact Information
		Name: Katie Hampson
		Institution: University of Glasgow-Scotland, UK
		Email: Katie.Hampson@glasgow.ac.uk
		
3. Date of data collection: From 2010-01-01 to 2019-08-31
4. Geographic location of data collection: South-eastern Tanzania in the regions of Lindi and Mtwara 
5. Information about funding sources that supported the collection of the data: This information has been described in the manuscript.

SHARING/ACCESS INFORMATION
1. Licenses/restrictions placed on the data: Identifying features such as names and GPS coordinates have been removed , Simulated coordinates would therefore be needed for some of the codes to run.
2. Links to publications that cite or use the data: NIL
3. Links to other publicly accessible locations of the data: https://github.com/LushasiK/Stz_rabies_reservoir
4. Links/relationships to ancillary data sets: NIL
5. Was data derived from another source? yes/no
	A. If yes, list source(s):  No.
6. Recommended citation for this dataset: Lushasi, Kennedy et al. (2021), Reservoir Dynamics of Rabies in Southeast Tanzania and the roles of cross-species transmission and domestic dog vaccination, Dryad, Dataset, https://doi.org/10.5061/dryad.bg79cnpbg

DATA & FILE OVERVIEW
The data files are described below:
1.	animal_bites: a list of biting animals with their unique identifier identified across the study areas. The three variable columns: i) species-shows a list of biting animals, ii) suspect- shows the healthy status of the biting animals, if suspected for rabies or not and iii) ID, which is the unique Identifier for the biting animal
2.	animal_cases_map: a list of animal rabies cases and their geographical coordinates indication the location of where they were identified. The variable columns include: i) year indicating the date of investigation, ii) Grp_species, indicates a list of animal species but with several wildlife species grouped together as wildlife, and the geographical locations of where the animal species were found, the Longitudes and Latitudes column headings.
3.	Case_numbers_all_areas: contains animal rabies cases from all areas across the study including northern Tanzania (Ngorongoro and Serengeti) and for Pemba Island. The variables in the column headings mean the following:  i) Year, the date of data collection, ii) species  column contains a list of different animals species, iii) Count is the number of animal species reported in that year, and iv) District shows the district location of where they were reported from.
4.	Cluster_assignment: details of cases assigned to individual clusters. The column variables, Region and district shows the locations of where animal cases were found, the species column shows the number of animal species, ID is the unique Identifier for the animal species, Symptoms. Started shows the date of when symptoms were first noticed in the animal, Cluster_No; shows the number of clusters the species belong to;  date_onset, shows the dates when animals started showing symptoms and the days_in are the number days from 2010-01-01 when the data was collected.
5.	Combined_animal_cases_ts: monthly cases of probable animal rabies. The variable column for months shows when date of when the case was reported, species columns show the animal species and n, shows the total number of species 
6.	Combined_human_deaths_ts: monthly probable human deaths according to the species of animal that caused the exposure. 
The column months shows when date case reported;  species columns shows the animal species;  the column n shows the total number of species  and y_value shows
7.	Combined_human_Exposure_ts: monthly probable human rabies exposures according to the species of animal that caused the exposure. The variable column for months shows when date of when a person was exposed to a suspected rabid animal, species columns show the biting animal species and n, shows the total number of human exposures.
8.	Dog_population_year: annual dog population estimate per ward
9.	exposure_incidence: Shows the number of people bitten by suspect rabid animals in each district by species. The estimated  human population of each district is also indicated in the variable column Pop.
10.	human_bites:  human bite data traced over the study period: The variable column for 
Attacking. Species shows animal species biting people, the column variable rabid shows the healthy status of the biting animals, Bitter. ID is the unique Identifier of the biting animal, while patient. Outcome shows the health status of the human bite victim. 
11.	human_exposures_prop: proportion of probable human rabies exposures by biting animal species.  Description of the column headings is as follows: Year  shows the date, Species is the list of biting animals reported, Spec_Count is the number of species reported in that period, while Total details the sum of all species reported in that year, and Prop_of_cases shows the proportions of animal cases reported.
12.	NBS2012_MasterVillagePop_shpmatched.csv: census data reporting the population by village. The district, ward and village shape files are also indicated in the variable columns and village level population data
13.	Number_of_dogs24: Estimates of dog numbers. The columns variables represent the following; District shows the location of areas/districts; Numdogs- is the estimated number of dogs; Numdogs.lwr is the lower limit of the estimated number of dogs; and Numdogs.upr is the estimated upper limits of the number of dogs.
14.	seren_dists: Distances between known cases from Serengeti. Used for parameter estimation
15.	seren_si: Serial intervals of known cases from Serengeti. Used for parameter estimation; The details for the column variables are as follows: ID is the unique identifier of the animal case; Biter_ID is the unique Identifier of the biting animal; SS is the date symptoms started in an infected animal case; SS_uncert, is the uncertainty around the dates of when symptoms started in infected animal case; biter_SS is the date symptoms started  in the biting animal; biter_SS_uncertainity is the uncertainity around the date symptoms started in the biting animal; and  Serial_Interval is the censoring interval
16.	STzCellData_4kmsq: 4kmsq grid cells of the study area with unique identifying number for each cell
17.	STzdog_pop_matrix: Dog population data matrix for the study site
18.	STzHumanPopMat_CellByMonth_2010-01-01_to_2020-12-31: Human population matrix for the study site in southern Tanzania from 2010 to 2020
19.	Vacc_coverage_summary_STzdists: Summary statistics of vaccination coverage by district across the study site
20.	VaccinationRounds: Shows annual intervals of vaccination campaigns
21.	Vaccinated_cleaned: Contains number of dogs vaccinated in each round of vaccination campaign in study districts
22.	Transects_cleaned: Dog vaccination transect data
23.	NBS2012_VillagePop_matched: Human population data for the study districts
24.	human_pop_growth: Estimated human population growth rate in the study districts
25.	EstimatedDogs2015: Contains estimated dog population and vaccination coverage of the study districts. The column variables Vdogs stands for the total count dogs from government survey data; vacc is the number of dogs vaccinated; unvacc is the number of unvaccinated dogs, tdogs is the total dogs, cov is the estimated vaccination coverage using dog population surveys from government census data; cov.est is the estimated vaccination coverage from transect data with the upper and lower confidence intervels (cov.ci.lo $ cov.ci.hi); est_dogs15 estimated dog population with the range of lower and upper estimates (low.est & high.est)
26.	Vaccination_coverage: Shows vaccination coverage in each round of vaccination campaign in the study districts, from round 1 of the campaign to the 5th round.


To run '6c.create_human_pop_Ngoro' and '6d.create_human_pop_Seren' the file 'data/tza_ppp_2012.tif' is called. This is not included due to size but can be downloaded from the WorldPop website (https://www.worldpop.org/)

Figures that are generated from the scripts and data are stored in subfolder: *figs*                                                                        
Output files generated from the scripts and data are stored in subfolder: *output*
Not all outputs are provided due to their size, but they can be created by running the scripts. 

The *R* subfolder contains helper functions for the scripts
The *Scripts* subfolder contains the R scripts


