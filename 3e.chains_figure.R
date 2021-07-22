
rm(list=ls())
library(lubridate)
library(plyr)

# Read in data 
chains <- read.csv(file = "data/cluster_assignment.csv", stringsAsFactors = FALSE); 
nrow(chains)
head(chains)


# Assign unique chains by district in descending order
districts = data.frame(District = c("Kilwa", "Liwale","Nachingwea", 
                                    "Ruangwa", "Lindi", 
                                    "Mtwara", "Tandahimba",
                                    "Newala", "Masasi", "Nanyumbu"), ID = seq(from=10000, to=1000, by=-1000))
chains$District_ID = districts$ID[match(chains$District, districts$District)] + chains$Cluster_No # Assign district IDs
clusters = data.frame(unique_chains = sort(unique(chains$District_ID)), IDs = 1:length(unique(chains$District_ID))) # create unique ordered clusters
chains$Cluster_ID = clusters$IDs[match(chains$District_ID, clusters$unique_chains)]


# In the species column - dogs are coded as 1, wildlife as 2 and cats as 4. 
colours = data.frame(col = c("red", "blue", "yellow"), code = c(1,2,4), spp = c("dog","wildlife","cat"))
chains$colours = as.character(colours$col[match(chains$Species, colours$code)])


# Days-in is the number of days between the start date (01/01/2011) and the date of symptoms onset. 
links <- chains %>%
  group_by(Cluster_ID) %>%
  summarise(min = min(days_in),
            max = max(days_in))

max_days = as.numeric(as.Date("2019-08-01") - as.Date("2011-01-01"))
yrs = seq(as.Date("2011-01-01"), as.Date("2019-08-01"), 'years') - as.Date("2011-01-01")


# Plot chains
pdf("Figs/chains_by_district.pdf")

plot(chains$days_in, chains$Cluster_ID, col = chains$colours, 
     pch=20, cex=0.95, xlab ="", ylab = "", axes=FALSE, xlim=c(-450,max_days))
axis(1, at = yrs, labels=2011:2019) # Rewrite as dates

# Links
for(i in 1:nrow(links)){
  lines(x = c(links$min[i], links$max[i]), y = c(links$Cluster_ID[i], links$Cluster_ID[i]))
}
points(chains$days_in, chains$Cluster_ID, col = chains$colours, pch=20, cex=0.95) # replot points to overlay links
points(chains$days_in[which(chains$Species=="1")], chains$Cluster_ID[which(chains$Species=="1")], 
       col = chains$colours[which(chains$Species=="1")], pch=20, cex=0.95) # just overlay dogs

# Put lines between districts
for(i in 1:nrow(districts)){
  y = min(chains$Cluster_ID[which(chains$District==districts$District[i])])-0.5
  lines(x=c(0,max_days), y=c(y,y), lty=2, lwd=0.5, col="dark grey")
  text(-300, y+2, districts$District[i], cex=0.85)
}

dev.off()

## png file with label C to combine with cluster plot for Figrue 4.
## Combined online using app 

png("figures/chains_by_district.png", width = 600, height = 450)

plot(chains$days_in, chains$Cluster_ID, col = chains$colours, 
     pch=20, cex=1.3, xlab ="", ylab = "", axes=FALSE, xlim=c(-450,max_days))
axis(1, at = yrs, labels=2011:2019) # Rewrite as dates
legend("topleft", legend = "C ", bty = "n")

# Links
for(i in 1:nrow(links)){
  lines(x = c(links$min[i], links$max[i]), y = c(links$Cluster_ID[i], links$Cluster_ID[i]))
}
points(chains$days_in, chains$Cluster_ID, col = chains$colours, pch=20, cex=1,3) # replot points to overlay links
points(chains$days_in[which(chains$Species=="1")], chains$Cluster_ID[which(chains$Species=="1")], 
       col = chains$colours[which(chains$Species=="1")], pch=20, cex=1.3) # just overlay dogs

# Put lines between districts
for(i in 1:nrow(districts)){
  y = min(chains$Cluster_ID[which(chains$District==districts$District[i])])-0.5
  lines(x=c(0,max_days), y=c(y,y), lty=2, lwd=0.5, col="dark grey")
  text(-300, y+2, districts$District[i], cex=1.1)
}

dev.off()

