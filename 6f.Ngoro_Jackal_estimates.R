## In this code we are looking at calculating jackal densities for Ngorongoro. 
## Assume no jackals in densely populated areas but assign them evenly over other areas#

rm(list = ls())

## Bring in the data from Elaine which is the cell info and the human population 
## estimates for each cell per month from 1/1/10 - 31/12/20

Cell_data <- read.csv("output/Ngoro_CellData4kmsq.csv")
HP <- read.csv("output/NgoroHumanPopMat_Cell.csv", header = F)

## Then create a data frame to house the info 

Area_DF <- as.data.frame(matrix(ncol =4, nrow =  1))
colnames(Area_DF) <- c("District", "Total_Area", "Area_below_2000_per_cell",
                       "Area_above0_below2000")

Totes_Area <- c()
Totes_Area <- nrow(Cell_data)*4

Area_out_2000 <- length(which(HP$V1<2000))*4

Area_out_abovezero_below2000 <- length(which(HP$V1<2000 & HP$V1 >0))*4

Area_out_above1_below2000 <- length(which(HP$V1<2000 & HP$V1 > 1))*4

Area_out_abovezero <- length(which(HP$V1 > 0))*4

Area_out_above1 <- length(which(HP$V1 >1 ))*4

Area_out_above5_below2000 <- length(which(HP$V1 >5 & HP$V1 <2000))*4

Area_out_above10_below2000 <- length(which(HP$V1<2000 & HP$V1 > 10))*4

Area_out_above20_below2000 <- length(which(HP$V1<2000 & HP$V1 > 20))*4


Area_DF[,"District"] <- "Ngorongoro"
Area_DF[,"Total_Area"] <- Totes_Area
Area_DF[,"Area_below_2000_per_cell"] <- Area_out_2000
Area_DF[,"Area_above0_below2000"] <- Area_out_abovezero_below2000 
Area_DF[,"Area_above1_below2000"] <- Area_out_above1_below2000
Area_DF[,"Area_above0"] <- Area_out_abovezero
Area_DF[,"Area_above1"] <-Area_out_above1
Area_DF[,"Area_above5_below2000"] <- Area_out_above5_below2000 
Area_DF[,"Area_above10_below2000"] <- Area_out_above10_below2000
Area_DF[,"Area_above20_below2000"] <- Area_out_above20_below2000 


#####

### Next need to use estimates of jackal density and apply to the selected areas to get 
### estimates of jackal numbers

## Will use a range of jackal density estimates due to uncertainty 
## Densities per sq km we will look at are 0.05, 0.15, 0.30

JD_function <- function(df){
  Out <- as.data.frame(matrix(ncol = 7, nrow = 1))
  colnames(Out) <- c("District", "Area_below_cutoff", "Jackals_using_density_0.05", 
                     "Jackals_using_density_0.15", "Jackals_using_density_0.30", "Jackals_using_density_0.50",
                     "Jackals_using_density_1")
  Out[1, "District"] <- "Ngorongoro"
  Out[1, "Area_below_cutoff"] <- df
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

 
# write.csv(JD_cutoff2000, "output/Ngoro_Jackal_number_estimates_cutoff2000percell.csv")
# write.csv(JD_cutoff_0_2000, "output/Ngoro_Jackal_number_est_above_0_below_2000_percell.csv")
# write.csv(JD_cutoff_1_2000, "output/Ngoro_Jackal_number_est_above_1_below_2000_percell.csv")
# write.csv(JD_cutoff_5_2000, "output/Ngoro_Jackal_number_est_above_5_below_2000_percell.csv")
# write.csv(JD_cutoff_10_2000, "output/Ngoro_Jackal_number_est_above_10_below_2000_percell.csv")
# write.csv(JD_cutoff_20_2000, "output/Ngoro_Jackal_number_est_above_20_below_2000_percell.csv")

