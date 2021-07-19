# Maximum Likelhood tree and various plots that also show the dog density
rm(list=ls())

library(spatstat)
library(maptools)
library(animation)
library(maps)
library(RColorBrewer)
library(GISTools)
library(rgdal)
library(rgeos)
library(codetools)

source("R/links.R")
source("R/Trees.R")

# Load shapefiles
region_shp <- readOGR("data/shps_pop", "TZ_Region_2012_pop")
district_shp <- readOGR("data/shps_pop", "TZ_District_2012_pop")
prot_areas_lindi <- readOGR("data/gis", "PA_Lindi")

# Subset shapefiles first the study area - Lindi and Mtwara Regions only
study_regs <- c("Lindi", "Mtwara")
study_reg <- region_shp[which(region_shp$Region_Nam %in% study_regs),]
study_dis <- district_shp[which(district_shp$Region_Nam %in% study_regs),]

# Import Animal contact tracing data - rabid animals only
animals <- read.csv("output/animal_cases.csv", stringsAsFactors = FALSE)

# Format as date
start_date <- as.Date("2011-01-01") # Set start date for data
animals$Symptoms.started <- as.Date(animals$Symptoms.started, format="%d-%b-%Y")
animals$Date.bitten <- as.Date(animals$Date.bitten, format="%d-%b-%Y") 
year(animals$Date.bitten)
year(animals$Symptoms.started)

# Subset data from 2011 to present
animalstz <- animalstz[which(animalstz$Symptoms.started >= start_date),]

# Group district names
table(animalstz$District)
animalstz$District[grepl("Lindi", animalstz$District)] <- "Lindi"
animalstz$District[grepl("Masasi", animalstz$District)] <- "Masasi"
animalstz$District[grepl("Mtwara", animalstz$District)] <- "Mtwara"
table(animalstz$District) # cHECK!

# Subset for only suspect or unknown suspect animals
animalstz <- animalstz[which(animalstz$Suspect == "Yes"),]

table(animalstz$Suspect)
 
# Group wildlife species as one
animalstz$Species_factor <- ifelse(grepl("Wildlife", animalstz$Species), "Wildlife",
                                   ifelse(animalstz$Species=="Domestic dog", "Domestic Dog", NA))
table(animalstz$Species_factor, useNA="always")

table(animalstz$Suspect, animalstz$Species_factor)
length(which(is.na(animalstz$Latitude))); length(which(is.na(animalstz$Longitude)))

# Read coordinate columns as numeric
animalstz$Latitude <- as.numeric(animalstz$Latitude)
animalstz$Longitude <- as.numeric(animalstz$Longitude)

# Subset by animal type
dogs_map <- animalstz[which(animalstz$Species_factor=="Domestic Dog"),]; nrow(dogs_map)
wildlife_map <- animalstz[which(animalstz$Species_factor=="Wildlife"),]; nrow(wildlife_map)


# plot a map for to show cases (dogs and wildlife)

plot(study_reg, lwd=0.5, add=F)
plot(study_dis, border="#474646", lwd=0.1, add=TRUE)
plot(prot_areas_lindi, col="darkgrey", border=FALSE, add=TRUE)

  points(x=dogs_map$Longitude, y=dogs_map$Latitude,
         cex=1, pch=19, col="Red")
  
  points(x=wildlife_map$Longitude, y=wildlife_map$Latitude,
         cex=1, pch=19, col="Blue")
 


  #Dx=c(639000,703000) ; SDy=c(9760000, 9835000) #Serengeti district limits
  

# Rabid data to match tree data (3365 rows with XY i.e. out of 3421)
start.date <- as.Date("2011-01-01")
animalstz$ds = as.numeric(animalstz$Symptoms.started - start.date)
animalstz$ms = ceiling(animalstz$ds/30.5) # hist(ct$ms[!is.na(ct$ms)], breaks=0:250)
dim(animalstz) # 3630 rabid records
table(animalstz$Rabid)

# Tree data
trees <- read.csv(file = "data/sources_1000KHKL.csv"); dim(trees) # 9772
LL = read.csv(file = "data/ll_1000KHKL.csv"); dim(LL)  # 3619

ML = numeric(ncol(LL)) # most likely tree
for(i in 1:ncol(LL)){ML[i]=sum(as.numeric(LL[,i]), na.rm=TRUE)}
MLtree=which(ML==max(ML))

###################################################################
# TROUBLESHOOT THE CASES THAT WERE NOT ASSIGNED PROGENITORS:
NAs=which(is.na(trees[,MLtree]))
NAs # 274  275 1011 1075 1241 2677 3365 3406 3420 3446 (10 out of 3630)
# Biter.ID 4194 has no ss date but ID 4192 and 4193 ss 2009-5-01
# Biter.ID 8696 was NOT suspect! but infected 8691
# Biter.ID 9121 has no UTMs but infected 9130
# ID 231 (1735) had symptoms on: 2002-01-07 (i.e. FIRST)
# Biter.ID 5332 was bitten twice! and infected 8678
# Biter.ID 9913 has no UTMS but infected 9919, 9920, 9935, 9939, 9940
# Biter.ID 10519 is UNKNOWN status but infected 10520
# Biter.ID 10465 is UNKNOWN status but infected 10466
# Biter.ID 10668 is UNKNOWN status but infected 10669

###################################################################

# Get the most likely progenitors and their space time distance
prog = rep(NA, nrow(animalstz))
prog_time <- prog_dist <- prog

for(i in 1:nrow(trees)){
  prog_group = get_prog(i, animalstz, trees) # Note that uncertainty in timing is NOT taken into account here (so get from trees direct)
  prog[i] = prog_group$progenitor
  prog_dist[i] = max(prog_group$distance, runif(1,1,100))
  prog_time[i] = max(1, prog_group$interval)
  print(i)
}
dist_breaks = seq(0,170000,1000); time_breaks = seq(0,150,1)
par(mfrow=c(1,2))
hist(prog_time, breaks=time_breaks)
hist(prog_dist, breaks=dist_breaks)

prog_time

# Compare to contact tracing
params= read.csv("output/params.csv")
n = nrow(rabid) # length(which(rabid$ds < end.day & rabid$Species == "Domestic dog")) # 2996 dog cases 2002-2015

int=250
ms = seq(0, 70000, int)
kms = ms/1000
prog_kernel = hist(prog_dist/1000, breaks=kms, plot=FALSE)$counts/n

par(mfrow=c(1,1))
plot(kms, int*dgamma(ms, shape = params$dist_shape, rate = params$dist_rate), 
     col="red", type="l", ylim=c(0,0.4), xlim=c(0,8)) #
points(kms[-1]-0.125, prog_kernel, pch=20)

threshold = 0.975
outlier = round((1-threshold)*n); outlier # 91 cases to be over threshold by chance (therefore include as outliers! Probably not incursions)
SI_crit = qgamma(threshold, shape = params$SI_shape, scale = params$SI_scale) # 97 days (118 days for 99%)
DK_crit = qgamma(threshold, shape = params$dist_shape, scale = 1/params$dist_rate) # 5.3 km (6.8 km for 99%)
outlier_long = sort(prog_time[which(prog_time>SI_crit)])[outlier]; outlier_long # 186 days!
outlier_far = sort(prog_dist[which(prog_dist>DK_crit)])[outlier]; outlier_far # 6.5 km!

SI_double_crit = quantile(rgamma(n, shape = params$SI_shape, scale = params$SI_scale) + 
                            rgamma(n, shape = params$SI_shape, scale = params$SI_scale), threshold)
DK_double_crit = quantile(rgamma(n, shape = params$dist_shape, scale = 1/params$dist_rate) + 
                            rgamma(n, shape = params$dist_shape, scale = 1/params$dist_rate), threshold)

long = (prog_time>SI_crit); sum(long, na.rm=TRUE)  #length(prog_time[which(prog_time>SI_crit)]) # 110 cases are over 97 days
far = (prog_dist>DK_crit); sum(far, na.rm = TRUE) #length(prog_dist[which(prog_dist>DK_crit)]) # 369 cases are over 5.3 km away
longer = (prog_time>outlier_long); sum(longer, na.rm=TRUE)  #length(prog_time[which(prog_time>SI_crit)]) # 19 cases are outliers
farther = (prog_dist>outlier_far); sum(farther, na.rm = TRUE) #length(prog_dist[which(prog_dist>DK_crit)]) # 295 cases are outliers
longest = (prog_time>SI_double_crit); sum(longest, na.rm=TRUE)  
farthest = (prog_dist>DK_double_crit); sum(farthest, na.rm = TRUE) 

incursions = (far | long); table(incursions) # 466 meet one or other criteria
incursions = (farther | longer); table(incursions) # 295 meet one or other criteria
incursions2 = (farthest | longest); table(incursions2) # 186 are ~2 transmission links away (5%)

rabid$progenitor = prog; table(is.na(rabid$progenitor)) # use NA in the loop (9 with NO progenitor)
rabid$prog_dist = prog_dist
rabid$prog_time = prog_time
rabid$incursions = incursions # Use true in the loop (note that NAs are unresolved)
rabid$incursions2 = incursions2 # Use true in the loop (note that NAs are unresolved)
# rabid$incursions[which(rabid$Biter.ID>0)] <- FALSE; table(rabid$incursions) # 407 incursions (464 inferred but 57 replaced because traced!)
rabid$incursions[which(rabid$Biter.ID>0)] <- FALSE; table(rabid$incursions) # 271 incursions (295 inferred but 24 replaced because traced!)
rabid$incursions2[which(rabid$Biter.ID>0)] <- FALSE; table(rabid$incursions2) # 200 incursions 
rabid$progenitor[which(rabid$incursions==TRUE)] <- NA
rabid$prog_check = rabid$Suspect[match(rabid$progenitor, rabid$ID)] # Checks
rabid$BiterID_check = rabid$Suspect[match(rabid$Biter.ID, rabid$ID)] # Checks
sum(incursions, na.rm=TRUE)/n # 8% cases are likely incursions

# Look at incursions over time
years = 2002:2020 # sequence for plotting
start = 1 #Jan 2002
end = 365*(length(years)-1)
months = 1:(12*(length(years)-1))
ys = seq(0, max(months), 12)
qs = seq(0, max(months), 3)
end_m <- floor(as.numeric(Sys.Date()-start.date)/30.5) # alternative check:

study_m = (2016-2002)*12
inc_ts_m = hist(rabid$ms[which(rabid$incursions == TRUE)], breaks=0:end_m, plot=FALSE)$counts
inc_ts_m2 = hist(rabid$ms[which(rabid$incursions2 == TRUE)], breaks=0:end_m, plot=FALSE)$counts
case_ts_m = hist(rabid$ms, breaks=0:end_m, plot=FALSE)$counts
inc_ts = hist(rabid$ms[which(rabid$incursions == TRUE)], breaks=seq(0, end_m+3, 1), plot=FALSE)$counts[1:(study_m/3)]
case_ts = hist(rabid$ms, breaks=seq(0, end_m+1, 3), plot=FALSE)$counts[1:(study_m/3)]

pdf("figs/incursions_v_cases.pdf")
par(mfrow=c(2,1), mgp = c(1.5,0.25,0), tck=-0.02, cex=0.75)
ts = rbind(inc_ts_m, case_ts_m - inc_ts_m)
mids <- barplot(ts, col = c("red", "grey"), border=NA)
axis(1, at = mids, labels =rep("", length(mids)), tck=-0.01)
axis(1, at = mids[seq(1, max(end_m), 12)], labels = 2002:2019, tck=-0.03)

ts = rbind(inc_ts_m2, case_ts_m - inc_ts_m2)
mids <- barplot(ts, col = c("red", "grey"), border=NA)
axis(1, at = mids, labels =rep("", length(mids)), tck=-0.01)
axis(1, at = mids[seq(1, max(end_m), 12)], labels = 2002:2019, tck=-0.03)

par(mfrow=c(2,1))
plot(case_ts_m[1:study_m], type="l", xlab="months", ylab="freq"); lines(inc_ts_m[1:study_m], col="red")
plot(inc_ts_m[1:study_m], type="l", ylim=c(0,5))

plot(case_ts, type="l", xlab="quarters", ylab="freq"); lines(inc_ts, col="red")
plot(inc_ts, type="l", ylim=c(0,5), xlab="months", ylab="freq")
dev.off()

# Write out incursion data:
# ID, biter.ID, ms, ds, Village, progenitor, incursions, prog_dist, prog_time, Date.bittn, Symptoms.started
check = data.frame(ID = rabid$ID, Biter.ID = rabid$Biter.ID,  vill = rabid$Village,
           ms = rabid$ms, ds = rabid$ds, dBitten = rabid$Date.bitten, dSymptoms = rabid$Symptoms.started, Suspect = rabid$Suspect,
           owner = rabid$Owner, spp = rabid$Species, progenitor = rabid$progenitor, incursions = rabid$incursions,  incursions2 = rabid$incursions2, 
           dist = rabid$prog_dist, interval = rabid$prog_time, prog_check = rabid$prog_check, biterID_check = rabid$BiterID_check)
write.csv(check[which(check$incursions == TRUE),], "output/incursion_check.csv", row.names = FALSE)
write.csv(check[(is.na(check$progenitor) & rabid$incursions == FALSE),], "output/unresolved_check.csv", row.names = FALSE)

#################################################################

# Plot pdf of unresolved and incursion data
pdf("figs/tree.pdf", width=9, height=12) # SHOW PDF WITH GREEN INCURSIONS
latent=dead=rdead=numeric(0)

# for (j in 1:study_m){
for (j in 1:end_m){
  par(mfrow=c(1,1), cex=0.6, cex=1.5, lwd=0.5, plt=c(0.05, 1, 0.05, 1), mgp=c(1.2,0.2,0), tck=-0.01, mar=c(1,2,2,2))
  plot(den, col = grey(seq(1, 0, length = max(DDkm))), box=FALSE, main="", ribbon=FALSE)
  plot(SVlines, add=TRUE, col="transparent", border="black", lwd=0.1)

  infectious=which(rabid$ms==j)
  if(length(infectious)>0){
    for(i in 1:length(infectious)){
      bitten=which(trees[,MLtree]==rabid$ID[infectious[i]])
      bitten=which(rabid$progenitor==rabid$ID[infectious[i]])
      for(k in 1:length(bitten)){
        x1=rabid$UTM.Easting[infectious[i]]; x2=rabid$UTM.Easting[bitten[k]]
        y1=rabid$UTM.Northing[infectious[i]]; y2=rabid$UTM.Northing[bitten[k]]
        lines(c(x1,x2), c(y1,y2), lwd="1", col="black", lty=1)
      }
      points(x2, y2, pch=20)
      latent=c(latent, bitten);
    }
  }
  dead=c(dead, infectious)

  #only plot recent deaths
  recent=(j-(3*4)):j
  rdead=dead[rabid$ms[dead] %in% recent]

  latent=latent[!latent %in% dead]
  points(rabid$UTM.Easting[latent], rabid$UTM.Northing[latent], pch=20)
  points(rabid$UTM.Easting[infectious], rabid$UTM.Northing[infectious], col="red", pch=20)

  incursions = which(rabid$ms==j & rabid$incursions==TRUE & rabid$incursions2!=TRUE)
  incursions2 = which(rabid$ms==j & rabid$incursions2==TRUE)
  unresolved = which(rabid$ms==j & is.na(rabid$progenitor) & rabid$incursions == FALSE)

  points(rabid$UTM.Easting[incursions], rabid$UTM.Northing[incursions], col="green", pch=20, cex=2)
  points(rabid$UTM.Easting[incursions2], rabid$UTM.Northing[incursions2], col="green", pch=8, cex=2)
  points(rabid$UTM.Easting[unresolved], rabid$UTM.Northing[unresolved], col="blue", pch=20, cex=2)
  mth = (j%%12)+1
  yr = (j%/%12)+2002
  text(650000, 9840000, paste(mth,"-",yr))
  # points(rabid$UTM.Easting[incursions], rabid$UTM.Northing[incursions], col="red", pch=22, cex=1.5)

  par(new=T, cex=0.75, lwd=0.5, plt=c(0.6, 0.93, 0.8, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
  # barplot(monthlycases[1:study_m], ylim=c(0,60), main="", ylab = "Cases", col="black")
  barplot(case_ts_m, ylim=c(0,60), main="", ylab = "Cases", col="black")
  axis(1, at=1.2*seq(0, end_m, 12), labels=c(2002:2019))
  mtext("cases", line=1.5, side=2, at=55, cex=0.5)
  arrows(j*1.2, 65, j*1.2, -10, col="red", lwd=2, length=0.05)
}
dev.off()


# Show links over population density
plot.serengeti.trees <- function(den, rabid, monthlycases) {

# pdf("figs/tree.pdf", width=9, height=12)
  study_m = (2016-2002)*12
  latent=dead=rdead=numeric(0)

for (j in 1:study_m){
#for (j in 1:end_m){
  par(mfrow=c(1,1), cex=0.6, cex=1.5, lwd=0.5, plt=c(0.05, 1, 0.05, 1), mgp=c(1.2,0.2,0), tck=-0.01, mar=c(1,2,2,2))
  plot(den, col = grey(seq(1, 0, length = max(DDkm))), box=FALSE, main="", ribbon=FALSE)
  plot(vill36, add=TRUE, col="transparent", border="black", lwd=0.1)

  infectious=which(rabid$ms==j)
  if(length(infectious)>0){
    for(i in 1:length(infectious)){
      bitten=which(trees[,MLtree]==rabid$ID[infectious[i]])
      bitten=which(rabid$progenitor==rabid$ID[infectious[i]])
      for(k in 1:length(bitten)){
        x1=rabid$UTM.Easting[infectious[i]]; x2=rabid$UTM.Easting[bitten[k]]
        y1=rabid$UTM.Northing[infectious[i]]; y2=rabid$UTM.Northing[bitten[k]]
        lines(c(x1,x2), c(y1,y2), lwd="1", col="black", lty=1)
      }
      points(x2, y2, pch=20)
      latent=c(latent, bitten);
    }
  }
  dead=c(dead, infectious)

  #only plot recent deaths
  recent=(j-(3*4)):j
  rdead=dead[rabid$ms[dead] %in% recent]

  latent=latent[!latent %in% dead]
  points(rabid$UTM.Easting[latent], rabid$UTM.Northing[latent], pch=20)
  points(rabid$UTM.Easting[infectious], rabid$UTM.Northing[infectious], col="red", pch=20)

  incursions = which(rabid$ms==j & is.na(rabid$progenitor))
  # points(rabid$UTM.Easting[incursions], rabid$UTM.Northing[incursions], col="green", pch=20, cex=2)
  points(rabid$UTM.Easting[incursions], rabid$UTM.Northing[incursions], col="red", bg="red", pch=22, cex=1.5)

  par(new=T, cex=0.75, lwd=0.5, plt=c(0.6, 0.93, 0.8, 0.95), mgp=c(1.2,0.2,0), tck=-0.01)
  barplot(monthlycases[1:study_m], ylim=c(0,60), main="", ylab = "Cases", col="black")
  # barplot(monthlycases, ylim=c(0,60), main="", ylab = "Cases", col="black")
  axis(1, at=1.2*seq(0, end_m, 12), labels=c(2002:2019))
  mtext("cases", line=1.5, side=2, at=55, cex=0.5)
  arrows(j*1.2, 65, j*1.2, -10, col="red", lwd=2, length=0.05)
}
# dev.off()
}

## save gif
findGlobals(plot.serengeti.trees, merge=FALSE)
ani.options(convert = "/opt/local/bin/convert")
setwd("/Users/katiehampson")
saveGIF({plot.serengeti.trees(den=den, rabid=rabid, monthlycases=monthlycases)},
        interval = 0.7, movie.name = "serengeti_trees.gif", ani.width = 800, ani.height = 1000, loop=1)




