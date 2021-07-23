rm(list = ls())

# Load libraries
library(tidyverse)
library(reshape2)
library(lattice)
library(grid)
library(latticeExtra)
library(gridExtra)
library(ggplotify)
library(cowplot)
library(ggpubr)
library(patchwork)
library(gridBase)
library(egg)
library(ggpmisc)
library(raster)
library(rgdal)
library(prettymapr)
library(maptools)

# Set options
options(stringsAsFactors = F)

# Load functions
source("ms_R/fig_label.R")

# Load data
animal_bites <- read.csv("ms_data/animal_bites.csv")
animal_cases_map <- read.csv("ms_data/animal_cases_map.csv")
animal_cases_ts <- read.csv ("ms_data/combined_animal_cases_ts.csv")
human_bites <- read.csv("ms_data/human_bites.csv")
human_exposures_ts <- read.csv("ms_data/combined_human_exposures_ts.csv")
human_deaths_ts <- read.csv("ms_data/combined_human_deaths_ts.csv")
human_exposures_prop <- read.csv("ms_data/human_exposures_prop.csv")
STzGrid <- raster("ms_data/gis/STzGrid4kmsq.grd")
cellData <- read.csv("ms_data/STzCellData_4kmsq.csv")
dogPopMat <- as.matrix(read.csv("ms_data/dog_population_year.csv", header=FALSE))
vacc_coverage <- read.csv("ms_data/vaccination_coverage.csv")
exposure_incidence <- read.csv("ms_data/exposure_incidence.csv")

# Load shapefiles
study_regions <- readOGR("ms_data/gis", "study_regions")
study_districts <- readOGR("ms_data/gis", "study_districts")
study_wards <- readOGR("ms_data/gis", "study_wards")
nearby_countries <- readOGR("ms_data/gis", "nearby_countries")
prot_areas_lindi <- readOGR("ms_data/gis", "PA_lindi")
prot_areas_mara <- readOGR("ms_data/gis", "PA_mara")

#----- TABLE 1 -----------------------------------------------------------------
# Probable animal rabies cases, human exposures and human rabies deaths by
# infecting species detected from January 2011 to July 2019 in Lindi and Mtwara
# regions.

# Summarise suspect animal cases
unique_sus_cases <- animal_bites %>%
  dplyr::select(Species, Suspect, ID) %>%
  distinct() %>%
  group_by(Species) %>%
  summarise(n_cases=n())

# Ssummarise suspect biting animals
unique_sus_bitinganimals <- human_bites %>%
  dplyr::select(Attacking.species, Rabid, Biter.ID) %>%
  distinct() %>%
  group_by(Attacking.species) %>%
  summarise(n_biters=n())

# Summarise exposures from rabid animals by attacking species
exposures <- human_bites %>%
  filter(Rabid == "Yes") %>%
  group_by(Attacking.species) %>%
  summarise(n_exp=n()) %>%
  mutate(p_exp=NA)

# Summarise deaths from rabid animals by attacking species
deaths <- human_bites %>%
  filter(Rabid == "Yes", Patient.outcome == "Died") %>%
  group_by(Attacking.species) %>%
  summarise(n_deaths=n()) %>%
  mutate(p_death=NA)

# Merge animal case and biting animal tables together
combined_tbl <- merge(unique_sus_cases, unique_sus_bitinganimals,
                      by.x="Species", by.y="Attacking.species", all=TRUE)

# Calculate proportion of cases from all animal records
combined_tbl$p_cases <- round(combined_tbl$n_cases/nrow(animal_bites)*100, digits=1)

# Merge human exposures and deaths in to table
combined_tbl <- merge(combined_tbl, exposures, by.x="Species", by.y="Attacking.species", all=TRUE)
combined_tbl <- merge(combined_tbl, deaths, by.x="Species", by.y="Attacking.species", all=TRUE)

# Change NA values to 0
combined_tbl[is.na(combined_tbl)] <- 0

# Calculate remaining proportions
combined_tbl$p_exp <- round((combined_tbl$n_exp/sum(combined_tbl$n_exp))*100, digits=1)
combined_tbl$p_death <- round((combined_tbl$n_deaths/sum(combined_tbl$n_deaths))*100, digits=1)

# Remove "Wildlife: " from wildlife species
combined_tbl$Species <- gsub("Wildlife: ", "", combined_tbl$Species)

# Build new table
summary_table <- data.frame(Animal_type = ifelse(combined_tbl$Species %in% c("Domestic dog", "Cat"), "Domestic animals","Wildlife"),
                            Species=combined_tbl$Species,
                            Cases=paste0(combined_tbl$n_cases, " (", combined_tbl$p_cases,")"),
                            Exposures=paste0(combined_tbl$n_exp, " (", combined_tbl$p_exp, ")"),
                            Deaths=paste0(combined_tbl$n_deaths, " (", combined_tbl$p_death, ")"),
                            stringsAsFactors = FALSE)

# Sort summary table by species
summary_table$Species <- factor(summary_table$Species,
                                levels=c("Domestic dog", "Cat", "Jackal", "Hyena", "Honey badger", "Leopard"))
summary_table <- arrange(summary_table, Species)

# name columns
colnames(summary_table) <- c("Type", "Species",
                             "Probable rabies cases (%)",
                             "Probable rabies exposures by species (%)",
                             "Deaths due to rabies by species (%)")

write.csv(summary_table, "ms_output/Table_1.csv", row.names=FALSE)

#----- FIGURE 2 ----------------------------------------------------------------
# Study districts and locations of probable rabies cases

# Set palette for map
breaks = seq(0, 1, length.out=10)
background_cols <- colorRampPalette(c("white", "dimgrey"))
background_cols <- background_cols(length(breaks)-1)

# Set labels and margins size
par(mai=c(0.1,0.1,0.2,0.1))

# Impose a minimum to help with logged colour scale bar
minDensity <- 0.1

# Begin saving plot code
pdf("ms_figs/Figure_2.pdf", height=14, width=10)

# Define area for plot 2a
par(fig=c(0,0.55,0.65,1))

# Produce plot 2a
plot(nearby_countries, border="dimgrey", lwd=1, col="white", xlim = c(34, 40), ylim = c(-12, 0))
plot(study_wards, col="#7e7d7d", border="#7e7d7d", add=TRUE)
plot(prot_areas_lindi, col="#d8d8d8", border=TRUE, add=TRUE, lty=2)
plot(prot_areas_mara, col="#d8d8d8", border=TRUE, add=TRUE, lty=2)
text(x=30.7, y=-10.8, labels="ZAMBIA", cex=0.9, col="dimgrey")
text(x=38, y=-1.5, labels="KENYA", cex=0.9, col="dimgrey")
text(x=35, y=-6, labels="TANZANIA", cex=0.9, col="dimgrey")
fig_label(text="A", cex=3)

# Add scalebar to plot 2a
addscalebar(pos="bottomleft")

# Create pop raster for last year
last_popGrid <- STzGrid
last_popGrid[which(last_popGrid[] == 0)] <- NA
last_popGrid[which(!is.na(last_popGrid[]))] <- dogPopMat[,2019 - 2011 + 2]/4
last_popGrid[which(!is.na(last_popGrid[]))[which(!cellData$Region %in% c("Lindi","Mtwara"))]] <- NA
last_popGrid <- projectRaster(last_popGrid, res = 0.01, crs = CRS("+proj=longlat"))
last_popGrid[which(last_popGrid[] < minDensity)] <-  minDensity

# Define area for plot 2b
par(fig=c(0.5,1,0.6,1), mar=c(0.2,0,0,0), new=TRUE)

# Produce plot 2b
plot(study_regions, lwd=1)
plot(log10(last_popGrid), col=background_cols, add = TRUE,
     breaks = seq(log10(minDensity), max(log10(dogPopMat), na.rm=T), length.out=10), legend=F)
par(fig=c(0.5,1,0.6,1), mar=c(0.2,0,0,0), new=TRUE)
plot(study_districts, border="#969696", lwd=0.05, add=TRUE)
plot(prot_areas_lindi, col="white", border="#969696", lty=2, add=TRUE)
plot(study_regions, lwd=1, add=TRUE)

# Position labels on plot 2b
districts_pos = coordinates(study_districts)
districts_pos[,2] = districts_pos[,2]-0.015
districts_pos[,1] = districts_pos[,1]+0.005
labels_districts = gsub("Masasi Township Authority", "", study_districts$District_N)
labels_districts = gsub("Lindi Urban", "", labels_districts)
labels_districts = gsub("Mtwara Urban", "", labels_districts)
labels_districts = gsub("Rural", "", labels_districts)
pointLabel(districts_pos, labels = labels_districts, cex = 0.8, bg="transparent")
fig_label(text="B", cex=3)

# Define area for plot 2c
locs <- data.frame(yr=c(2011:2019),
                   xmin=c(0,0.33,0.66, 0,0.33,0.66, 0,0.33,0.66),
                   xmax=c(0.33,0.66,1, 0.33,0.66,1, 0.33,0.66,1),
                   ymin=c(0.4,0.4,0.4, 0.2,0.2,0.2, 0,0,0),
                   ymax=c(0.6,0.6,0.6, 0.4,0.4,0.4, 0.2,0.2,0.2))

for(i in 2011:2019){

  # Subset dataframe row for plot position
  loc_row <- locs[which(locs$yr==i),]

  par(fig=c(loc_row$xmin, loc_row$xmax, loc_row$ymin, loc_row$ymax),
      mar=c(0,0,0.5,1), new=TRUE) # Set margins

  # Subset data
  dogs_map_sub <- animal_cases_map[which(animal_cases_map$Grp_species=="Domestic Dog" & animal_cases_map$Year==i),]
  wildlife_map_sub <- animal_cases_map[which(animal_cases_map$Grp_species=="Wildlife" & animal_cases_map$Year==i),]

  ## Create population raster for this year
  popGrid <- STzGrid
  popGrid[which(popGrid[] == 0)] <- NA
  popGrid[which(!is.na(popGrid[]))] <- dogPopMat[, i - 2011 + 2]/4
  popGrid[which(!is.na(popGrid[]))[which(!cellData$Region %in% c("Lindi","Mtwara"))]] <- NA
  popGrid <- projectRaster(popGrid, res = 0.01, crs = CRS("+proj=longlat"))
  popGrid[which(popGrid[]<minDensity)] <-  minDensity

  # add shape files
  plot(study_regions, lwd=1)
  plot(log10(popGrid), col=background_cols, add=TRUE,
       breaks=seq(log10(minDensity), max(log10(dogPopMat), na.rm=T), length.out=10), legend=F)


  # Plot after the raster to show bondaries properly
  par(fig=c(loc_row$xmin, loc_row$xmax, loc_row$ymin, loc_row$ymax),
      mar=c(0,0,0.5,1), new=TRUE) # Set margins
  plot(study_regions, lwd=1, add=TRUE)
  plot(study_districts, border="#474646", lwd=0.1, add=TRUE)
  plot(prot_areas_lindi, col="white", border=TRUE, add=TRUE,lty=2)
  title(main=i, cex.main=1.5, line = -1)

  # add points
  points(x=dogs_map_sub$Longitude, y=dogs_map_sub$Latitude, cex=1, pch=21, bg=alpha("red", 0.7), col=NA)
  points(x=wildlife_map_sub$ Longitude, y=wildlife_map_sub$ Latitude, cex=1, pch=21, bg=alpha("blue", 0.7), col=NA)
  if(i == 2011){ fig_label(text="C", cex=3) }
}

# Add legend for plot 2c
par(fig=c(0.25, 0.70, 0.6,0.7), new=TRUE)
plot(study_regions, lwd=1, border="white")
legend(41, -8, legend=c("Dogs", "Wildlife"), cex=1, bty = "n",
       pt.cex=1.5, pch=21, pt.bg=c(alpha("red", 0.7), alpha("blue", 0.7)), col=NA)
ticks <- seq(log10(minDensity), log10(1000), 1)
plot(log10(popGrid), breaks=seq(log10(minDensity),max(log10(dogPopMat),na.rm=T),length.out=10),
     legend.only=T, add=T, col=background_cols,
     legend.args=list(text=expression(paste("Dogs/km"^2)), side=4, font=2, line=4.2, cex=0.9),
     axis.args=list(at=ticks,labels=c(eval(substitute(expression(""<= m), list(m = minDensity))),10^ticks[2:length(ticks)])),cex.axis=1.1,
     smallplot=c(0.36,0.38, .25,.85))

# Save output
dev.off()

#----- FIGURE 3 ----------------------------------------------------------------
# Probable animal rabies cases, human rabies exposures and deaths by species from
# January 2011 to July 2019

# Set factor levels
animal_cases_ts$species <- factor(animal_cases_ts$species, levels = c("other_wild", "jackal", "other_dom", "dog"))
human_exposures_prop$Species <- factor(human_exposures_prop$Species, levels=c("Dog", "Wildlife"))
human_exposures_prop$Year <- factor(human_exposures_prop$Year, levels=2011:2019)

# Set labels and breaks
month_breaks <- 0:max(animal_cases_ts$month)
species_labels <- c("Other wildlife", "Jackals", "Other domestic", "Dogs")
date_labels <- 2011:2019
yr_breaks = seq(from=0, to=max(month_breaks), by=12)

# Create colour palette
own_cols <- c("other_wild"=alpha("blue", 0.5), "jackal"="blue", "other_dom"=alpha("red", 0.5), "dog"="red")

# Produce plot 3a
fig_3a <- ggplot() +
  geom_line(data=human_exposures_ts, aes(x=month, y=n, group=species, color=species)) +
  geom_point(data=human_deaths_ts, aes(x=month, y=y_value, group=species, color=species, size=n)) +
  scale_color_manual(name="Species", values=c("red", "blue"), breaks=c("Dog", "Wildlife")) +
  #***** RS: This is how we control the variable size of the points
  scale_size(name="Deaths", breaks=sort(unique(human_deaths_ts$n))) +
  scale_x_continuous(labels = date_labels, breaks = yr_breaks) +
  labs(y = "Monthly exposures and deaths", x = "", title= "") +
  geom_hline(yintercept=0, linetype="dashed", size=0.2) + # 0
  theme_classic() +
  theme(axis.text.x  = element_text(size = 16, face = "plain"),
        axis.title.x = element_text(size=16, face="plain"),
        axis.text.y = element_text(size = 16, face = "plain"),
        axis.title.y = element_text(size=16, face="plain"),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 14, face = "plain"),
        legend.title = element_text(size = 16, face = "plain"),
        legend.position = c(0.8,0.7))

# Produce plot 3b
fig_3b <- ggplot(animal_cases_ts, aes(x = month, y = n, fill = species))  +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = own_cols, name = "Species", labels = species_labels,
                    guide = guide_legend(reverse=TRUE)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16, face="plain"),
        axis.title.y = element_text(size=16, face="plain"),
        axis.text.x  = element_text(size = 16, face = "plain"),
        axis.text.y = element_text(size = 16, face = "plain"),
        axis.ticks.length = unit(.2, "cm"),
        legend.text = element_text(size = 16, face = "plain"),
        legend.title = element_text(size = 18, face = "plain"),
        legend.position = c(0.9,0.7)) +
  labs(title = "", y = "Monthly cases", x = "") +
  scale_x_continuous(labels = date_labels, breaks = yr_breaks) +
  geom_vline(xintercept=c(2, 18, 42, 56, 71), linetype="dotted", size=1)

# Produce inset for plot 3
fig_3c <- ggplot() +
  geom_line(data=human_exposures_prop, aes(x=Year, y=Prop_of_cases, group=Species, color=Species), size=1) +
  scale_color_manual(name="Species", values=c("red", "blue"), breaks=c("Dog", "Wildlife")) +
  labs(y = "Proportion", x = "", title = "") + #, title = "Proportion of human exposures per species per year") +
  theme_classic() +
  theme(axis.text.x  = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        legend.position = "none",
        axis.ticks.length = unit(.2, "cm"))

# Combine plots 3a and 3c (inset)
plot.with.inset <-
  ggdraw() +
  draw_plot(fig_3a) +
  draw_plot(fig_3c, x = 0.4, y = .5, width = .35, height = .45) +
  draw_plot_label(label="C", x=0.37, y=0.95, size = 28)

# Combine plots 3a (+ inset) with 3b
plot_grid(plot.with.inset,
          fig_3b,
          ncol=1,
          labels=LETTERS[1:2],
          align=c("l","r"),
          label_size = 24)

# Save output
ggsave("ms_figs/Figure_3.pdf", height=10, width=14)

#----- TABLE S1 ----------------------------------------------------------------
# Dog vaccination coverage by district

# Transform proportions to percentage
vacc_coverage = vacc_coverage %>%
  mutate_at(vars(Round_1:Round_5), .funs = funs(.*100))

# Calculate mean, sd, median and range for each round to 1dp
col_mean = round(sapply(vacc_coverage[2:6], mean), digits=1)
col_sd = round(sapply(vacc_coverage[2:6], sd), digits=1)
col_median = round(sapply(vacc_coverage[2:6], median), digits=1)
col_range = round(sapply(vacc_coverage[2:6], range), digits=1)

# Round proportions to 1dp
vacc_coverage = vacc_coverage %>%
  mutate_at(vars(Round_1:Round_5), funs(as.character(round(., digits=1))))

# Arrange dataframe by district
vacc_coverage$District = factor(vacc_coverage$District,
                                levels=c("Kilwa", "Lindi Rural", "Lindi Urban",
                                         "Liwale", "Masasi", "Masasi Township Authority",
                                         "Mtwara Rural", "Mtwara Urban", "Nachingwea",
                                         "Nanyumbu", "Newala", "Ruangwa", "Tandahimba"))
vacc_coverage = arrange(vacc_coverage, District)

# Add mean, sd, median and range as new rows
final_vacc_coverage <- vacc_coverage %>%
  add_row(District = "Mean (standard deviation)",
          Round_1 = paste0(col_mean[1], " (", col_sd[1], ")"),
          Round_2 = paste0(col_mean[2], " (", col_sd[2], ")"),
          Round_3 = paste0(col_mean[3], " (", col_sd[3], ")"),
          Round_4 = paste0(col_mean[4], " (", col_sd[4], ")"),
          Round_5 = paste0(col_mean[5], " (", col_sd[5], ")")) %>%
  add_row(District = "Median (range)",
          Round_1 = paste0(col_median[1], " (", col_range[1,1], "-", col_range[2,1], ")"),
          Round_2 = paste0(col_median[2], " (", col_range[1,2], "-", col_range[2,2], ")"),
          Round_3 = paste0(col_median[3], " (", col_range[1,3], "-", col_range[2,3], ")"),
          Round_4 = paste0(col_median[4], " (", col_range[1,4], "-", col_range[2,4], ")"),
          Round_5 = paste0(col_median[5], " (", col_range[1,5], "-", col_range[2,5], ")"))

# Save output
write.csv(final_vacc_coverage, "ms_output/Table_S2.csv", row.names=FALSE)

#----- TABLE S3 ----------------------------------------------------------------
# Incidence of human rabies exposures

# We only have 7/12 (months of data for?) 2019 so change 2019 population to reflect this
exposure_incidence$pop_adj = 0
exposure_incidence$pop_adj <- ifelse(exposure_incidence$Year == "2019",
                                     exposure_incidence$Pop * (7/12), exposure_incidence$Pop)

# Sum the population and exposures by district
incidence_table <- exposure_incidence[, 2:7]
incidence_table <- aggregate(. ~ District, incidence_table, sum)

# Calculate incidence rate per 100000 person years
incidence_table$per_hun <- round((incidence_table$Total_exposures/incidence_table$pop_adj)*100000, digits=1)
incidence_table$Dom_per_hun <- round((incidence_table$Domestic/incidence_table$pop_adj)*100000, digits=1)
incidence_table$Wild_per_hun <- round((incidence_table$Wildlife/incidence_table$pop_adj)*100000, digits=1)

# Finalise table to only some columns
final_incidence_table = incidence_table %>%
  dplyr::select(District, "Exposures_per_100000"=per_hun,
                "Exposures_from_domestic_per_100000"=Dom_per_hun,
                "Exposures_from_wild_per_100000"=Wild_per_hun)

# Save output
write.csv(final_incidence_table, "ms_output/Table_S3.csv", row.names=FALSE)
