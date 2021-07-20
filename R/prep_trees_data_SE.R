
data <- read.csv("output/animal_cases.csv")

SE_Tanz_2011 <- data
SE_Tanz_2011$Date_started <- as.Date(SE_Tanz_2011$Symptoms.started, format = "%Y-%m-%d")

# Ensure have only include cases that have entries for the symptoms started and the long/lat
SE_Tanz_2011 <-SE_Tanz_2011[complete.cases(SE_Tanz_2011$Symptoms.started),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Latitude),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Longitude),]

# Take only suspect = Yes
SE_Tanz_2011 <- SE_Tanz_2011[which(SE_Tanz_2011$Suspect == "Yes"),]


extract_uncertainty = function(uncertainty){
  temp1 <- gregexpr("[0-9]+", uncertainty)  # Numbers with any number of digits
  as.numeric(unlist(regmatches(uncertainty, temp1))) # extract digits
}

SE_Tanz_2011$SI_accuracy <- extract_uncertainty(SE_Tanz_2011$Symptoms.started.accuracy)



