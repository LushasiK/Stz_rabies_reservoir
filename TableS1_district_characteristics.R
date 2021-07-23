# File to generate Table S1

rm(list = ls())

library(tidyverse)
library(rgdal)

## Bring in the data from is the cell info and the human population 
## estimates for each cell per month from 1/1/10 - 31/12/20

Cell_data <- read.csv("data/STzCellData_4kmsq.csv")
HP <- read.csv("data/STzHumanPopMat_CellByMonth_2010-01-01_to_2020-12-31.csv")

Cell_data_ngoro <- read.csv("output/Ngoro_CellData4kmsq.csv")
HP_ngoro <- read.csv("output/NgoroHumanPopMat_Cell.csv", header = F)

Cell_data_seren <- read.csv("output/Seren_CellData4kmsq.csv")
HP_seren <- read.csv("output/SerenHumanPopMat_Cell.csv", header = F)

HP_pemba <- read.csv("output/humanPopMatPembaCell4kmsq.csv")

# Check that we have the districts we are expected and that there are no spelling issues
# Then select the ones in STz. 
levels(Cell_data$District)
table(Cell_data$District)

D_names <- c("Kilwa", "Lindi Rural", "Lindi Urban", "Liwale", "Masasi", "Masasi Township Authority", 
             "Mtwara Rural", "Mtwara Urban", "Nachingwea", "Nanyumbu", "Newala", "Ruangwa", "Tandahimba")

# Then create a data frame to house the info 
Area_DF <- as.data.frame(matrix(ncol =3, nrow =  length(D_names)))
colnames(Area_DF) <- c("District", "Total_Area", "Area_above0")

Totes_Area <- c()
for (i in 1:length(D_names)) {
  Totes_Area[i] <- length(which(Cell_data$District== D_names[i]))*4 # multiplying
  #by 4 as cells are 4sq km. So count number of cells then multiply by 4
  Totes_Area
}


## USing column 25 from HP as this will correspond to Jan 2012 (census year)

Area_out_abovezero <-c()
for (i in 1:length(D_names)) {
  Area_out_abovezero[i] <- length(which(Cell_data$District== D_names[i] & 
                                          HP[,25] > 0))*4 
  Area_out_abovezero
}

Area_DF[,"District"] <- D_names
Area_DF[,"Total_Area"] <- Totes_Area
Area_DF[,"Area_above0"] <- Area_out_abovezero

## Then create a data frame to house the info 

Totes_Area_ngoro <- c()
Totes_Area_ngoro <- nrow(Cell_data_ngoro)*4
Area_out_abovezero_ngoro <- length(which(HP_ngoro$V1 > 0))*4

Totes_Area_seren <- c()
Totes_Area_seren <- nrow(Cell_data_seren)*4
Area_out_abovezero_seren <- length(which(HP_seren$V1 > 0))*4

Totes_Area_pemba <- c()
Totes_Area_pemba <- nrow(HP_pemba)*4
Area_out_abovezero_pemba <- length(which(HP_pemba$V1 > 0))*4


Area_DF[14, "District"] <- "Ngorongoro"
Area_DF[14, "Total_Area"] <- Totes_Area_ngoro
Area_DF[14, "Area_above0"] <- Area_out_abovezero_ngoro

Area_DF[15, "District"] <- "Pemba"
Area_DF[15, "Total_Area"] <- Totes_Area_pemba
Area_DF[15, "Area_above0"] <- Area_out_abovezero_pemba

Area_DF[16, "District"] <- "Serengeti"
Area_DF[16, "Total_Area"] <- Totes_Area_seren
Area_DF[16, "Area_above0"] <- Area_out_abovezero_seren



#### Human population

# Data from census

Area_DF$Pop[Area_DF$District == "Kilwa"] <- 190744
Area_DF$Pop[Area_DF$District == "Lindi Rural"] <- 194143
Area_DF$Pop[Area_DF$District == "Lindi Urban"] <- 78841
Area_DF$Pop[Area_DF$District == "Liwale"] <- 91380
Area_DF$Pop[Area_DF$District == "Masasi"] <-247993
Area_DF$Pop[Area_DF$District == "Masasi Township Authority"] <- 102696 
Area_DF$Pop[Area_DF$District == "Mtwara Rural"] <- 228003
Area_DF$Pop[Area_DF$District == "Mtwara Urban"] <- 108299
Area_DF$Pop[Area_DF$District == "Nachingwea"] <- 178464
Area_DF$Pop[Area_DF$District == "Nanyumbu"] <- 150857
Area_DF$Pop[Area_DF$District == "Newala"] <- 205492
Area_DF$Pop[Area_DF$District == "Ruangwa"] <- 131080 
Area_DF$Pop[Area_DF$District == "Tandahimba"] <- 227514
Area_DF$Pop[Area_DF$District == "Serengeti"] <- 249420
Area_DF$Pop[Area_DF$District == "Ngorongoro"] <- 174278
Area_DF$Pop[Area_DF$District == "Pemba"] <- 211732+195116

Lindi_region <- Area_DF$District %in% c("Liwale", "Kilwa", "Lindi Rural","Lindi Urban", "Nachingwea",
                                         "Ruangwa")
Mtwara_region <- Area_DF$District %in% c("Masasi", "Masasi Township Authority", "Mtwara Rural",
                                          "Mtwara Urban", "Nanyumbu", "Newala", "Tandahimba")

# multiply by region specific growth rates
Area_DF[Lindi_region, "Pop18" ] <- Area_DF[Lindi_region, "Pop" ]*1.009^6
Area_DF[Mtwara_region , "Pop18" ] <- Area_DF[Mtwara_region, "Pop" ]*1.012^6
Area_DF[which(Area_DF$District == "Ngorongoro" ), "Pop18"] <- 
  Area_DF[which(Area_DF$District == "Ngorongoro"), "Pop"]*1.027^6
Area_DF[which(Area_DF$District == "Pemba" ), "Pop18"] <- (211732*1.013^6)+(195116*1.011^6)
Area_DF[which(Area_DF$District == "Serengeti" ), "Pop18"] <- 
  Area_DF[which(Area_DF$District == "Serengeti"), "Pop"]*1.025^6

######

# Dog numbers

dogs <- read.csv("data/Number_of_dogs24.csv")
dogs$District <- str_to_title(dogs$District)

table(dogs$District)
dogs[which(dogs$District == "Masasi Urban"), "District"] <- "Masasi Township Authority"

All_names <- c("Kilwa", "Lindi Rural", "Lindi Urban", "Liwale", "Masasi", "Masasi Township Authority", 
             "Mtwara Rural", "Mtwara Urban", "Nachingwea", "Nanyumbu", "Newala", "Ruangwa", "Tandahimba",
             "Ngorongoro", "Serengeti")

Pemba_names <- c("Wete", "Micheweni", "Chakechake", "Mkoani")
head(dogs)

dogs_pemba <- dogs[which(dogs$District %in% Pemba_names), c("District", "Numdogs", "Numdogs.lwr", "Numdogs.upr")]
dogs_pemba <- dogs_pemba %>%
  dplyr::select(!District) %>%
  colSums()

dogs<- dogs[which(dogs$District %in% All_names), c("District", "Numdogs", "Numdogs.lwr", "Numdogs.upr")]

s1_table <- left_join(Area_DF, dogs)
s1_table[which(s1_table$District == "Pemba"),c("Numdogs", "Numdogs.lwr", "Numdogs.upr")] <- dogs_pemba


## rabies cases

rabs <- read.csv("data/Case_numbers_all_areas.csv")
rabs <- dplyr::select(rabs, -X)
head(rabs)

rabs_wide <- pivot_wider(rabs, names_from = Species, values_from = Count, values_fill = 0)
head(rabs_wide)
rabs_wide$all_dom <- rabs_wide$`Domestic dog` + rabs_wide$Livestock + rabs_wide$Cat
rabs_wide$all_wild <-rabs_wide$Jackal + rabs_wide$Wildlife

rabs_sum <- 
  rabs_wide %>%
    group_by(District) %>%
    summarise(totes_dom = sum(all_dom), totes_wild = sum(all_wild))

rabs_sum[which(rabs_sum$District == "Masasi  Township Authority"),"District"] <- "Masasi Township Authority"

s1_table <- left_join(s1_table, rabs_sum)  


write.csv(s1_table, "output/Table_s1.csv")
