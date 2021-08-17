GENERAL INFORMATION
1. Title of Dataset: Reservoir Dynamics of Rabies in Southeast Tanzania and the roles of cross-species transmission and domestic dog vaccination

2. Author Information

	A.Investigator Contact Information                                                                                                                                                    Name: Kennedy Lushasi
	     Institution: Ifakara Health Institute, Dar es salaam-Tanzania
	     Email: klushasi@ihi.or.tz

	B.  Investigator Contact Information                                                                                                                                           	      Name: Sarah Hayes
             Institution:  Imperial College, London-UK
	     Email: sarah.hayes16@imperial.ac.uk

	C. Alternate Contact Information                                                                                                                                                     Name: Katie Hampson
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
1.	animal_bites: a list of biting animals with their unique identifier identified across the study areas. The three variable columns: i) Species - shows a list of biting animals, ii) Suspect - shows the health status of the biting animals (if suspected for rabies or not) and iii) ID - the unique identifier for the biting animal
2.	animal_cases_map: a list of animal rabies cases and their geographical coordinates indicating the location where they were identified. The variable columns include: i) Year - the date of investigation, ii) Grp_species - a list of animal species but with several wildlife species grouped together as wildlife, iii) Longitude and and iv)  Latitude - the geographical locations of where the animals were found.
3.	Case_numbers_all_areas: contains animal rabies cases from all areas across the study including northern Tanzania (Ngorongoro and Serengeti) and for Pemba Island. The variables in the column headings mean the following:  i) Year - the date of data collection, ii) Species - a list of different animals species, iii) Count - the number of animal species reported in that year, and iv) District - the district from which the case was reported.
4.	cluster_assignment: details of cases assigned to individual clusters. The column variables included are: i) Region and ii) District - the locations of where animal cases were found at regional- and district-level respectively, iii) ID - the unique identifier for the animal, iv) Species - the animal species, v) Symptoms.Started - the date on which symptoms were first noticed in the animal, vi) Cluster_No - the number of the cluster to which the animal was assigned, vii) date_onset - the date on which the animal started showing symptoms and viii) days_in - the number of days from 2011-01-01 when the animal started showing symptoms.
5.	combined_animal_cases_ts: monthly counts of probable animal rabies cases. The variable columns are: i) month - the month when the case was reported with month 1 being January 2011, ii) species - the animal species and iii) n - the total number of cases in that month in that species 
6.	combined_human_deaths_ts: monthly number of human deaths due to rabies according to the species of animal that caused the rabies exposure. The column variables are i) month - the month when the death was reported with month 1 being January 2011, ii) species - the animal species iii) n - the total number of deaths for that month and species and iv) y_value - used during production of figure for manuscript
7.	combined_human_exposures_ts: monthly probable human rabies exposures according to the species of animal that caused the exposure. The column variables are: i) month - the month when when the exposure occured with month 1 being January 2011, ii) species - the species of the biting animal and iii) n - the total number of human exposures by month and species.
8.	exposure_incidence: Shows the number of people bitten by probable rabid animals in each district by species. The estimated  human population of each district is also indicated in the variable column Pop.
9.	human_bites:  human bite data traced over the study period. The columns variables are: i) Attacking.Species - the species of the biting animal, ii) Rabid - whether the animal was considered likely to be rabid, iii) Bitter. ID - the unique identifier of the biting animal and iv) Patient.Outcome shows the health status outcome of the human bite victim. 
10.	human_exposures_prop: proportion of probable human rabies exposures by year and biting animal species. The column variables are: i) Year - the year during which the exposures occurred, ii) Species - the species responsible for the exposures, iii) Spec_Count - the number of exposures from that species reported in that year, iv) Total - the sum of all exposures by all species reported in that year, and v) Prop_of_cases - the proportion of the total exposures from that year that were caused by the particular species.
11.	NBS2012_MasterVillagePop_shpmatched.csv: census data reporting the human population by village, ward and distrit. The columns labelled shpDistrict, shpWard and shpVillage correspond to the names in an associated shapefile.
12.	Number_of_dogs24: Estimates of dog numbers. The columns variables represent the following; District shows the location of areas/districts; Numdogs- is the estimated number of dogs; Numdogs.lwr is the lower limit of the estimated number of dogs; and Numdogs.upr is the estimated upper limits of the number of dogs.
13.	seren_dists: Distances between known domestic dog rabies cases from Serengeti. Used for parameter estimation
14.	seren_si: Serial intervals of known domestic dog rabies cases from Serengeti. Used for parameter estimation. The column variables are: i) ID - the unique identifier of the animal case, ii) Biter_ID - the unique identifier of the biting animal iii) SS - the date symptoms started in an infected animal case, iv) SS_uncert - the uncertainty, in days, around the date of when symptoms started in an infected animal case, v) biter_SS - the date symptoms started  in the biting animal, vi) biter_SS_uncertainty - the uncertainty, in days, around the date symptoms started in the biting animal, and vii) Serial_Interval - the serial interval not incorporating any of the reported uncertainty.
15.	STzCellData_4kmsq: 4kmsq grid cells of the study area with unique identifying number for each cell
16.	STzdog_pop_matrix: Dog population data matrix for the study site
17.	STzHumanPopMat_CellByMonth_2010-01-01_to_2020-12-31: Human population matrix for the study site in southern Tanzania from 2010 to 2020
18.	Vacc_coverage_summary_STzdists: Summary statistics of vaccination coverage by district across the study site. The columns variables are: i) District- Is the name for the study area district, ii)min_vacc- is the  minumum vaccination coverage per district, iii) max_vacc- is the maximum vaccination coverage for the study districts, iv) row_mean- is the mean vaccination coverage for the study area, and v) rom_median- represents the median vaccination coverage for the district under study.
19.	VaccinationRounds: Shows annual intervals of vaccination campaigns. The column headings shows the start and end dates of each vaccination rounds from the 1st to the 5th round of the vaccination campaigns.
20.	Vaccinated_cleaned: Contains number of dogs vaccinated in each round of vaccination campaign in study districts. The column variables include: i) correct_village- is the village name, ii) correct_ward- is the ward name, iii) correct_District- is the district name, iv) Correct_Region- is the region for the study area, v)dateVaccination- is the date when vaccination campaign was conducted, and vi Dogs- is the number of dogs vaccinated on that date in that particular area.
21.	Transects_cleaned: Dog vaccination transect data. The column variables include: i) Date-is the date when transects were conducted, ii) correct_village- is the village name, iii) correct_ward- is the ward name, iv) correct_District- is the district name, v) Correct_Region- is the region for the study area, vi)Dogs.without.collars- dogs encountered during transect walk without marked collars, vii) Dogs.with.collars- dogs encountered with marked vaccination collars during transect walk, viii) month- number of months from Jan 2011, ix) year- Year of when transects were conducted, x) DWV- stands for the combined district, ward and village names, xi) DW-Represents a combination of district and ward name.
22.	NBS2012_VillagePop_matched: Human population data for the study districts. The column variables shows the region, district, ward and village names from the national bureau of statistics population census survey of 2012 matched with the region, district, ward and village shape files.The human population estimates per 2012 is incicated on the column variable, population.
23.	human_pop_growth: Estimated human population growth rate in the study districts
24.	EstimatedDogs2015: Contains estimated dog population and vaccination coverage of the study districts. The column variables Vdogs stands for the total count dogs from government survey data; vacc is the number of dogs vaccinated; unvacc is the number of unvaccinated dogs, tdogs is the total dogs, cov is the estimated vaccination coverage using dog population surveys from government census data; cov.est is the estimated vaccination coverage from transect data with the upper and lower confidence intervels (cov.ci.lo $ cov.ci.hi); est_dogs15 estimated dog population with the range of lower and upper estimates (low.est & high.est)
25.	Vaccination_coverage: Shows vaccination coverage in each round of vaccination campaign in the study districts, from round 1 of the campaign to the 5th round.


To run '6c.create_human_pop_Ngoro' and '6d.create_human_pop_Seren' the file 'data/tza_ppp_2012.tif' is called. This is not included due to size but can be downloaded from the WorldPop website (https://www.worldpop.org/)

Figures that are generated from the scripts and data are stored in subfolder: *figs*                                                                        
Output files generated from the scripts and data are stored in subfolder: *output*
Not all outputs are provided due to their size, but they can be created by running the scripts. 

The *R* subfolder contains helper functions for the scripts
The *Scripts* subfolder contains the R scripts


