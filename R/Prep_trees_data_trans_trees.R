#rm(list =ls())

data <- read.csv("output/animal_cases.csv", stringsAsFactors = F)

SE_Tanz_2011 <- data

SE_Tanz_2011$Date_started <- as.Date(SE_Tanz_2011$Symptoms.started, format = "%Y-%m-%d")
#SE_Tanz_2011$Date_started <- as.Date(SE_Tanz_2011$Symptoms.started, format = "%d/%m/%Y")

## Just have cases that have entries for the symptoms started and the long/lat
SE_Tanz_2011 <-SE_Tanz_2011[complete.cases(SE_Tanz_2011$Symptoms.started),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Latitude),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Longitude),]

SE_Tanz_2011 <- SE_Tanz_2011[which(SE_Tanz_2011$Suspect == "Yes"),]

### Need columns for uncertainty calculations for SIs

extract_uncertainty = function(uncertainty){
  temp1 <- gregexpr("[0-9]+", uncertainty)  # Numbers with any number of digits
  as.numeric(unlist(regmatches(uncertainty, temp1))) # extract digits
}

SE_Tanz_2011$SI_accuracy <- extract_uncertainty(SE_Tanz_2011$Symptoms.started.accuracy)


table(SE_Tanz_2011$Species)
SE_Tanz_2011$Species[grep("Wildlife", SE_Tanz_2011$Species)] <- "Wildlife"
SE_Tanz_2011$Species[grep("Domestic dog", SE_Tanz_2011$Species)] <- "Dog"
table(SE_Tanz_2011$Species)

## Need to add a time difference as a numeric on to the data set for use within the function

start <- as.Date("2011-01-01")  

SE_Tanz_2011$Time_diff <- SE_Tanz_2011$Date_started - start

SE_Tanz_2011$Time_diff <- as.numeric(SE_Tanz_2011$Time_diff)

##There are some cases in this with NA for the animal ID. 

SE_Tanz_2011[(!complete.cases(SE_Tanz_2011$ID)),]

rabid <- SE_Tanz_2011

## Need to add a column for the month since the start of data collection
rabid$month <- as.numeric(format(rabid$Date_started,  "%m"))
rabid$year <- as.numeric(format(rabid$Date_started, "%Y"))

rabid$months_since_start <- rep(0, length(nrow(rabid)))

for(i in 1:(nrow(rabid))){
  if(rabid$year[i] == "2011"){
    rabid$months_since_start[i] = rabid$month[i]
  } else { 
    if(rabid$year[i] == "2012"){
      rabid$months_since_start[i] = rabid$month[i]+12
    } else { 
      if(rabid$year[i] == "2013"){
        rabid$months_since_start[i] = rabid$month[i]+24
      } else { 
        if(rabid$year[i] == "2014"){
          rabid$months_since_start[i] = rabid$month[i]+36
        } else { 
          if(rabid$year[i] == "2015"){
            rabid$months_since_start[i] = rabid$month[i]+48
          } else { 
            if(rabid$year[i] == "2016"){
              rabid$months_since_start[i] = rabid$month[i]+60
            } else { 
              if(rabid$year[i] == "2017"){
                rabid$months_since_start[i] = rabid$month[i]+72
              } else { 
                if(rabid$year[i] == "2018"){
                  rabid$months_since_start[i] = rabid$month[i]+84
                } else { 
                  if(rabid$year[i] == "2019"){
                    rabid$months_since_start[i] = rabid$month[i]+96
                  } else { 
                    NULL}
              }}}}}}}}}


table(rabid$months_since_start)

