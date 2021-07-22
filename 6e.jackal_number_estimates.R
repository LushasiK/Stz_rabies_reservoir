## In this code we are looking at calculating jackal densities. 
## Assume no jackals in densely populated areas but assign them evenly over other areas#

rm(list = ls())

## Bring in the data from Elaine which is the cell info and the human population 
## estimates for each cell per month from 1/1/10 - 31/12/20

Cell_data <- read.csv("data/STzCellData_4kmsq.csv")
HP <- read.csv("data/STzHumanPopMat_CellByMonth_2010-01-01_to_2020-12-31.csv")

# Check that we have the districts we are expected and that there are no spelling issues
# Then select the ones in STz. 
levels(Cell_data$District)
table(Cell_data$District)

D_names <- c("Kilwa", "Lindi Rural", "Lindi Urban", "Liwale", "Masasi", "Masasi Township Authority", 
             "Mtwara Rural", "Mtwara Urban", "Nachingwea", "Nanyumbu", "Newala", "Ruangwa", "Tandahimba")


# Select a cut off for a human population density above which wouldn't expect to see any jackals 

# First look at some of the densitiesin the different areas.

#range(HP[which(Cell_data$District == "Newala"),1])
#hist(HP[which(Cell_data$District == "Newala"),1], breaks = seq(0,8000,100))

range(HP[which(Cell_data$District == "Liwale"),25])
hist(HP[which(Cell_data$District == "Liwale"), 25], breaks = seq(0,6600,10))
Liwale <- HP[which(Cell_data$District == "Liwale"),25]
table(Liwale)

range(HP[which(Cell_data$District == "Kilwa"),25])
hist(HP[which(Cell_data$District == "Kilwa"), 25], breaks = seq(0,5500,100))
Kilwa <- HP[which(Cell_data$District == "Kilwa"), 25]
table(Kilwa)

#range(HP[which(Cell_data$District == "Mtwara Urban"),1])
#hist(HP[which(Cell_data$District == "Mtwara Urban"),1], breaks = seq(0,16500,100))


## Then create a data frame to house the info 

Area_DF <- as.data.frame(matrix(ncol =4, nrow =  length(D_names)))
colnames(Area_DF) <- c("District", "Total_Area", "Area_below_2000_per_cell",
                       "Area_above0_below2000")

Totes_Area <- c()
for (i in 1:length(D_names)) {
  Totes_Area[i] <- length(which(Cell_data$District== D_names[i]))*4 # multiplying
  #by 4 as cells are 4sq km in Elaines model which is where the tables we are 
  #using are from. So count number of cells then multiply by 4
  Totes_Area
}


## USing column 25 from HP as this will correspond to Jan 2012 

Area_out_2000 <- c()
for (i in 1:length(D_names)) {
  Area_out_2000[i] <- length(which(Cell_data$District== D_names[i] & HP[,25]< 2000))*4 
  Area_out_2000
}

Area_out_abovezero_below2000 <-c()
for (i in 1:length(D_names)) {
  Area_out_abovezero_below2000[i] <- length(which(Cell_data$District== D_names[i] & 
                                                    HP[,25] > 0 & HP[,25]< 2000))*4 
  Area_out_abovezero_below2000
}


Area_out_above1_below2000 <-c()
for (i in 1:length(D_names)) {
  Area_out_above1_below2000[i] <- length(which(Cell_data$District== D_names[i] & 
                                                    HP[,25] > 1 & HP[,25]< 2000))*4 
  Area_out_above1_below2000
}


Area_out_abovezero <-c()
for (i in 1:length(D_names)) {
  Area_out_abovezero[i] <- length(which(Cell_data$District== D_names[i] & 
                                                 HP[,25] > 0))*4 
  Area_out_abovezero
}

Area_out_above1 <-c()
for (i in 1:length(D_names)) {
  Area_out_above1[i] <- length(which(Cell_data$District== D_names[i] & 
                                                 HP[,25] > 1 ))*4 
  Area_out_above1
}

Area_out_above5_below2000 <-c()
for (i in 1:length(D_names)) {
  Area_out_above5_below2000[i] <- length(which(Cell_data$District== D_names[i] & 
                                                 HP[,25] > 5 & HP[,25]< 2000))*4 
  Area_out_above5_below2000
}

Area_out_above10_below2000 <-c()
for (i in 1:length(D_names)) {
  Area_out_above10_below2000[i] <- length(which(Cell_data$District== D_names[i] & 
                                                 HP[,25] > 10 & HP[,25]< 2000))*4 
  Area_out_above10_below2000
}

Area_out_above20_below2000 <-c()
for (i in 1:length(D_names)) {
  Area_out_above20_below2000[i] <- length(which(Cell_data$District== D_names[i] & 
                                                 HP[,25] > 20 & HP[,25]< 2000))*4 
  Area_out_above20_below2000
}

Area_DF[,"District"] <- D_names
Area_DF[,"Total_Area"] <- Totes_Area
Area_DF[,"Area_below_2000_per_cell"] <- Area_out_2000
Area_DF[,"Area_above0_below2000"] <- Area_out_abovezero_below2000 
Area_DF[,"Area_above1_below2000"] <- Area_out_above1_below2000
Area_DF[,"Area_above0"] <- Area_out_abovezero
Area_DF[,"Area_above1"] <-Area_out_above1
Area_DF[,"Area_above5_below2000"] <- Area_out_above5_below2000
Area_DF[, "Area_above10_below2000"] <- Area_out_above10_below2000
Area_DF[, "Area_above20_below2000"] <- Area_out_above20_below2000

#write.csv(Area_DF, "Output/Jackal_pop_size_estimates.csv")

#####

### Next need to use estimates of jackal density and apply to the selected areas to get 
### estimates of jackal numbers

## Will use a range of jackal density estimates due to uncertainty 
## Densities per sq km we will look at are 0.05, 0.15, 0.30


## Modified function so that can add estimates for Serengeti and Ngorongoro 
## that were generated in separate files

JD_function <- function(df){
  Out <- as.data.frame(matrix(ncol = 7, nrow = 13))
  colnames(Out) <- c("District", "Area_below_cutoff", "Jackals_using_density_0.05", 
                                          "Jackals_using_density_0.15", "Jackals_using_density_0.30",
                     "Jackals_using_density_0.50", "Jackals_using_density_1")
  Out[1:13,"District"] <- D_names
  #Out[14, "District"] <- "Serengeti"
  #Out[15, "District"] <- "Ngorongoro"
  Out[1:13, "Area_below_cutoff"] <- df
  #Out[14, "Area_below_cutoff"] <- 3296
  #Out[15, "Area_below_cutoff"] <- 15508
  Out[,"Jackals_using_density_0.05"] <- Out[,"Area_below_cutoff"]*0.05
  Out[,"Jackals_using_density_0.15"] <- Out[,"Area_below_cutoff"]*0.15
  Out[,"Jackals_using_density_0.30"] <- Out[,"Area_below_cutoff"]*0.30
  Out[,"Jackals_using_density_0.50"] <- Out[,"Area_below_cutoff"]*0.50
  Out[,"Jackals_using_density_1"] <- Out[,"Area_below_cutoff"]*1
  Out
}

JD_cutoff2000 <- JD_function(Area_DF$Area_below_2000_per_cell)
JD_cutoff_0_2000 <- JD_function(Area_DF$Area_above0_below2000)
JD_cutoff_1_2000 <- JD_function(Area_DF$Area_above1_below2000)
JD_cutoff_5_2000 <- JD_function(Area_DF$Area_above5_below2000)
JD_cutoff_10_2000 <- JD_function(Area_DF$Area_above10_below2000)
JD_cutoff_20_2000 <- JD_function(Area_DF$Area_above20_below2000)

##Add in that Pemba has zero jackals for future use

Pemba_jackals <- as.data.frame(matrix(nrow = 1, ncol = 7))
colnames(Pemba_jackals) <- c("District", "Area_below_cutoff", "Jackals_using_density_0.05", 
                   "Jackals_using_density_0.15", "Jackals_using_density_0.30", "Jackals_using_density_0.50", 
                   "Jackals_using_density_1")

Pemba_jackals[,1:7] <- c("Pemba", NA, 0,0,0,0,0)

JD_cutoff2000 <-rbind(JD_cutoff2000, Pemba_jackals)
JD_cutoff_0_2000 <- rbind(JD_cutoff_0_2000, Pemba_jackals)
JD_cutoff_1_2000 <- rbind(JD_cutoff_1_2000, Pemba_jackals)
JD_cutoff_5_2000 <- rbind(JD_cutoff_5_2000, Pemba_jackals)
JD_cutoff_10_2000 <- rbind(JD_cutoff_10_2000, Pemba_jackals)
JD_cutoff_20_2000 <- rbind(JD_cutoff_20_2000, Pemba_jackals)


## Read in the serengeti and ngorongoro info

Ngoro2000 <- read.csv("output/Ngoro_Jackal_number_estimates_cutoff2000percell.csv")
Ngoro_0_2000 <- read.csv("output/Ngoro_Jackal_number_est_above_0_below_2000_percell.csv")
Ngoro_1_2000 <- read.csv("output/Ngoro_Jackal_number_est_above_1_below_2000_percell.csv")
Ngoro_5_2000 <- read.csv("output/Ngoro_Jackal_number_est_above_5_below_2000_percell.csv")
Ngoro_10_2000 <- read.csv("output/Ngoro_Jackal_number_est_above_10_below_2000_percell.csv")
Ngoro_20_2000 <- read.csv("output/Ngoro_Jackal_number_est_above_20_below_2000_percell.csv")


Seren2000 <- read.csv("output/Serengeti_Jackal_number_estimates_cutoff2000percell.csv")
Seren_0_2000 <- read.csv("output/Serengeti_Jackal_number_est_above_0_below_2000_percell.csv")
Seren_1_2000 <- read.csv("output/Serengeti_Jackal_number_est_above_1_below_2000_percell.csv")
Seren_5_2000 <- read.csv("output/Serengeti_Jackal_number_est_above_5_below_2000_percell.csv")
Seren_10_2000 <- read.csv("output/Serengeti_Jackal_number_est_above_10_below_2000_percell.csv")
Seren_20_2000 <- read.csv("output/Serengeti_Jackal_number_est_above_20_below_2000_percell.csv")


JD_cutoff2000 <-rbind(JD_cutoff2000, Ngoro2000[,2:8], Seren2000[, 2:8])
JD_cutoff_0_2000 <- rbind(JD_cutoff_0_2000, Ngoro_0_2000[,2:8], Seren_0_2000[, 2:8])
JD_cutoff_1_2000 <- rbind(JD_cutoff_1_2000, Ngoro_1_2000[,2:8], Seren_1_2000[, 2:8])
JD_cutoff_5_2000 <- rbind(JD_cutoff_5_2000, Ngoro_5_2000[,2:8], Seren_5_2000[, 2:8])
JD_cutoff_10_2000 <- rbind(JD_cutoff_10_2000, Ngoro_10_2000[,2:8], Seren_10_2000[, 2:8])
JD_cutoff_20_2000 <- rbind(JD_cutoff_20_2000, Ngoro_20_2000[,2:8], Seren_20_2000[, 2:8])



# write.csv(JD_cutoff2000, "output/Jackal_number_estimates_cutoff2000percell.csv")
# write.csv(JD_cutoff_0_2000, "output/Jackal_number_est_above_0_below_2000_percell.csv")
# write.csv(JD_cutoff_1_2000, "output/Jackal_number_est_above_1_below_2000_percell.csv")
# write.csv(JD_cutoff_5_2000, "output/Jackal_number_est_above_5_below_2000_percell.csv")
# write.csv(JD_cutoff_10_2000, "output/Jackal_number_est_above_10_below_2000_percell.csv")
# write.csv(JD_cutoff_20_2000, "output/Jackal_number_est_above_20_below_2000_percell.csv")


