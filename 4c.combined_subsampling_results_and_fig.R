# In this document we are combining results from the subsampling analyses. 

rm(list = ls())

library(lattice)
library(reshape2)
library(ggplot2)
library(ggpubr)


# Bring in the data and reduce to just the 4 common categories
Sixty <- read.csv("output/subsampling_results_60_50_WR_below99.csv")
Sixty["SS_percent"] <- 60

Seventyfive <- read.csv("output/subsampling_results_75_50_WR_below99.csv")
Seventyfive["SS_percent"] <- 75

Ninety <-  read.csv("output/subsampling_results_90_50_WR_below99.csv")
Ninety["SS_percent"] <- 90

Hundred <-  read.csv("output/subsampling_results_100_50_WR_below99.csv")
Hundred["SS_percent"] <- 100

Combo <- rbind(Hundred, Ninety, Seventyfive, Sixty)

# combine the data to allow comparison of the different levels
# Plot these on a grouped barplot
# make data frame to use in plot

Stacked <- Combo[, c("Transmission",  "percent_median", "SS_percent")]
Stack_m <- Stacked[which(Stacked$Transmission == "DD" |Stacked$Transmission == "DW" |Stacked$Transmission == "WD" | Stacked$Transmission == "WW"),]
Trans_labs <- c("Dog-Dog", "Dog-Wildlife", "Wildlife-Dog", "Wildlife-wildlife")

Stack_m$SS_percent <- as.factor(Stack_m$SS_percent)

par(mar = c(5,5,0,0))

## make data frame to use in plot

Stack_error <- Combo[, c("Transmission",  "percent_median", "percent_low","percent_high", "SS_percent")]

Stack_ME <- Stack_error[which(Stack_error$Transmission == "DD" |
                                Stack_error$Transmission == "DW" |Stack_error$Transmission == "WD" | Stack_error$Transmission == "WW"),]

Stack_ME$SS_percent <- as.factor(Stack_ME$SS_percent)

# Look at the levels per available dog

Combo_rpt <- Combo
source("Prep_trees_data_SE.R")
table(SE_Tanz_2011$Species)
## There are 303 dogs and 221 jackals in the full set. Wildlife total is 236
## 10 cats

303*0.9 # 272.7
303*0.75 # 227.25
303*0.6 # 181.8

Combo_rpt["divisor"] <- 0


Combo_rpt[which(Combo_rpt$SS_percent == 100 & (Combo_rpt$Transmission == "CD" |
                                                 Combo_rpt$Transmission == "DD" | Combo_rpt$Transmission == "WD")),"divisor"] <- 303

Combo_rpt[which(Combo_rpt$SS_percent == 90 & (Combo_rpt$Transmission == "CD" |
                                                Combo_rpt$Transmission == "DD" | Combo_rpt$Transmission == "WD")),"divisor"] <- 273

Combo_rpt[which(Combo_rpt$SS_percent == 75 & (Combo_rpt$Transmission == "CD" |
                                                Combo_rpt$Transmission == "DD" | Combo_rpt$Transmission == "WD")),"divisor"] <- 227

Combo_rpt[which(Combo_rpt$SS_percent == 60 & (Combo_rpt$Transmission == "CD" |
                                                Combo_rpt$Transmission == "DD" | Combo_rpt$Transmission == "WD")),"divisor"] <- 182


## wlidlife and cat transmissions don't change across the percentages
Combo_rpt[which(Combo_rpt$Transmission == "CW" |
                  Combo_rpt$Transmission == "DW" |
                  Combo_rpt$Transmission == "WW"),"divisor"] <- 236

Combo_rpt[which(Combo_rpt$Transmission == "CC" |
                  Combo_rpt$Transmission == "DC" |
                  Combo_rpt$Transmission == "WC"),"divisor"] <- 10

Common_trans <- Combo_rpt[which(Combo_rpt$Transmission == "DD" | Combo_rpt$Transmission == "DW"| Combo_rpt$Transmission == "WD" | Combo_rpt$Transmission == "WW"),]

Common_trans["Per_animal"] <- Common_trans[, "Median"]/Common_trans[,"divisor"]
Common_trans["Lower_CI_per_animal"] <- Common_trans[, "Low_quant"]/Common_trans[, "divisor"]
Common_trans["Upper_CI_per_animal"] <- Common_trans[, "High_quant"]/Common_trans[,"divisor"]


# Alternative would be to divide by the number of infecting animals available. So divide by the number of the animals making up the first group in the transmission. ie for DD and DW divide by dog number and for WD and WW divide by dog number. 

Combo_rpt2 <- Combo_rpt

Combo_rpt2["divisor"] <- 0

Combo_rpt2[which(Combo_rpt2$SS_percent == 100 & (Combo_rpt2$Transmission == "DC" |
                                                   Combo_rpt2$Transmission == "DD" | Combo_rpt2$Transmission == "DW")),"divisor"] <- 303

Combo_rpt2[which(Combo_rpt2$SS_percent == 90 & (Combo_rpt2$Transmission == "DC" |
                                                  Combo_rpt2$Transmission == "DD" | Combo_rpt2$Transmission == "DW")),"divisor"] <- 273

Combo_rpt2[which(Combo_rpt2$SS_percent == 75 & (Combo_rpt2$Transmission == "DC" |
                                                  Combo_rpt2$Transmission == "DD" | Combo_rpt2$Transmission == "DW")),"divisor"] <- 227

Combo_rpt2[which(Combo_rpt2$SS_percent == 60 & (Combo_rpt2$Transmission == "DC" |
                                                  Combo_rpt2$Transmission == "DD" | Combo_rpt2$Transmission == "DW")),"divisor"] <- 182


## wlidlife and cat transmissions don't change across the percentages
Combo_rpt2[which(Combo_rpt2$Transmission == "WC" |
                   Combo_rpt2$Transmission == "WW" |
                   Combo_rpt2$Transmission == "WD"),"divisor"] <- 236

Combo_rpt2[which(Combo_rpt2$Transmission == "CW" |
                   Combo_rpt2$Transmission == "CD" |
                   Combo_rpt2$Transmission == "CC"),"divisor"] <- 10

Common_trans2 <- Combo_rpt2[which(Combo_rpt2$Transmission == "DD" | Combo_rpt2$Transmission == "DW"| Combo_rpt2$Transmission == "WD" | Combo_rpt2$Transmission == "WW"),]

Common_trans2["Per_animal"] <- Common_trans2[, "Median"]/Common_trans2[,"divisor"]
Common_trans2["Lower_CI_per_animal"] <- Common_trans2[, "Low_quant"]/Common_trans2[, "divisor"]
Common_trans2["Upper_CI_per_animal"] <- Common_trans2[, "High_quant"]/Common_trans2[,"divisor"]

# And now plot

## make data frame to use in plot

Stacked2 <- Common_trans2[, c("Transmission",  "Per_animal", "SS_percent", "Lower_CI_per_animal", "Upper_CI_per_animal")]

Stack_m2 <- Stacked2[which(Stacked2$Transmission == "DD" |Stacked2$Transmission == "DW" |Stacked2$Transmission == "WD" | Stacked2$Transmission == "WW"),]

Trans_labs <- c("Dog-Dog", "Dog-Wildlife", "Wildlife-Dog", "Wildlife-wildlife")


Stack_m2$SS_percent <- as.factor(Stack_m2$SS_percent)

# Export a two plot panel for inclusion in paper



own_cols_plot4 <- c("slategray1", "slategray2", "slategray3", "slategray4")

plot3 <- ggplot(Stack_ME, aes(factor(Transmission), percent_median, fill = SS_percent)) + 
  geom_bar(stat="identity", position = "dodge") + 
  geom_errorbar(aes(ymin = percent_low, ymax = percent_high), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values = own_cols_plot4, name = "Percentage of dogs",
                    guide = guide_legend(reverse=TRUE), 
                    labels = c("60%", "75%", "90%", "100%")) +
  theme_classic() +   theme(axis.title.x = element_text(size=14, face="bold"), 
                            axis.title.y = element_text(size=14, face="bold"), 
                            axis.text.x  = element_text(size = 12, face = "bold"), 
                            axis.text.y = element_text(size = 12, face = "bold"),
                            legend.text = element_text(size = 12, face = "bold"), 
                            legend.title = element_text(size = 12, face = "bold"),
                            legend.position = "right" ) + #c(0.8,0.7)) +
  labs(title = "", 
       y = "Median percent of transmission", x = "Type of transmission") + 
  scale_x_discrete(labels = c("D-D", "D-W", "W-D", "W-W"))

plot4 <- ggplot(Stack_m2, aes(factor(Transmission), Per_animal, 
                              fill = SS_percent)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values = own_cols_plot4, name = "Percentage of dogs",
                    guide = guide_legend(reverse=TRUE), 
                    labels = c("60%", "75%", "90%", "100%")) +
  geom_errorbar(aes(ymin = Lower_CI_per_animal, ymax = Upper_CI_per_animal), width=.2,position=position_dodge(.9)) +
  theme_classic() +   theme(axis.title.x = element_text(size=12, face="bold"), 
                            axis.title.y = element_text(size=12, face="bold"), 
                            axis.text.x  = element_text(size = 12, face = "bold"),
                            axis.text.y = element_text(size = 12, face = "bold"),
                            legend.text = element_text(size = 12, face = "bold"), 
                            legend.title = element_text(size = 12, face = "bold"),
                            legend.position = "right" ) +
  labs(title = "", 
       y = "Proportion of available transmissions", x = "Type of transmission") + 
  scale_x_discrete(labels = c("D-D", "D-W", "W-D", "W-W"))

png("rplot.png", width = 800, height = 350)
# 2. Create the plot

ggarrange(plot3, plot4, #+ rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

dev.off()
