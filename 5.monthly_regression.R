#-----------------------------------------
# Author: Sarah Hayes
# Title: Regression looking at trend in monthly cases
# First part of the code is taken from KL Incidence plots. 
# This is the code for the plot of the 3 panel plot of the chosen regressions for the monthly case data. 

#-------------------------------------------

rm(list = ls())

# Load libraries
library(tidyverse) # Note tidyverse already includes: tidyr, dplyr, lubridate, ggplot2, tibble
library(reshape2)
library(lattice)
library(grid)
library(latticeExtra)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(MASS)
library(gridBase)


# Load data
animal <- read.csv ("output/animal_cases.csv")

# ANIMAL DATA ------------------------------------------------------------------

# Look at number of cases in each species in each subset
Animal_cases <- as.data.frame(table(animal$Species))
colnames(Animal_cases) <- c("Species", "N_cases")
Animal_cases$percentage <- round((Animal_cases$N_cases/sum(Animal_cases$N_cases)*100), digits=1)

# Now need number of cases per month; Time series
month_breaks <- 0:((12*length(unique(animal$Year)))-5)
rabies_ts = hist(animal$month_n, breaks=month_breaks, plot=TRUE)$counts

sum(rabies_ts)

## quick check to make sure correct.
nrow(animal[which(animal$month == 1 & animal$Year == 2011),])
nrow(animal[which(animal$month_n == 1),])

nrow(animal[which(animal$month == 4 & animal$Year == 2011),])
nrow(animal[which(animal$month_n == 4),])
## they appear to be matching up ok.


wild_jackal <- subset(animal, animal$Species == "Wildlife: Jackal")
wild_other <- subset(animal, animal$Species %in% c("Wildlife: Honey badger", "Wildlife: Hyena", "Wildlife: Leopard"))
dom_dog <- subset(animal, animal$Species == "Domestic dog")
dom_other <- subset(animal, animal$Species == "Cat")

## make into time series
wild_jackal_ts = hist(wild_jackal$month_n, breaks=month_breaks, plot=TRUE)$counts
wild_other_ts = hist(wild_other$month_n, breaks=month_breaks, plot=TRUE)$counts
dom_dog_ts = hist(dom_dog$month_n, breaks=month_breaks, plot=TRUE)$counts
dom_other_ts = hist(dom_other$month_n, breaks=month_breaks, plot=TRUE)$counts

## make data frame to use in plot
combined_animal_cases <- data.frame(month = month_breaks[2:length(month_breaks)],
                                    dog = dom_dog_ts,
                                    other_dom = dom_other_ts,
                                    jackal = wild_jackal_ts,
                                    other_wild = wild_other_ts,
                                    stringsAsFactors = FALSE)

# add the total counts by month
combined_animal_cases$total <- rabies_ts
combined_animal_cases$month_new <- combined_animal_cases$month - 1

newx <- as.data.frame(combined_animal_cases$month_new)
colnames(newx) <- "month_new"

## Regression models for the monthly animal case data. 
## As we have count data may need to use a Poisson model or a negative binomial

combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 80 < 0, 0, combined_animal_cases$month_new - 79) 

newx_spline <- as.data.frame(combined_animal_cases$spline_bit)
colnames(newx_spline) <- "new_spline_bit"

####################################################
### Negative binomial model 

mod_sp_nb <- glm.nb(total ~ month_new + spline_bit, data = combined_animal_cases)
summary(mod_sp_nb)
exp(coef(mod_sp_nb))

AIC(mod_sp_nb)

ndata_spline_nb <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(mod_sp_nb,
                                                                           list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                           se.fit = TRUE)[1:2]),
                                                         c('fit_link','se_link')))

ilink_sp_nb <- family(mod_sp_nb)$linkinv

year_labs <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
pos_labs <- c(0, 12, 24, 36, 48, 60, 72, 84, 96)

## create the interval and backtransform
ndata_spline_nb <- mutate(ndata_spline_nb,
                          fit_resp  = ilink_sp_nb(fit_link),
                          right_upr = ilink_sp_nb(fit_link + (2 * se_link)),
                          right_lwr = ilink_sp_nb(fit_link - (2 * se_link)))


##############################################################################################################
###   Repeat for the separate species.
###   Start with domestic animals.

combined_animal_cases$all_dom <- combined_animal_cases$dog + combined_animal_cases$other_dom

combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 80 < 0, 0, combined_animal_cases$month_new - 79) 


##### Negative binomial model

dom_sp_nb <- glm.nb(all_dom ~ month_new + spline_bit, data = combined_animal_cases)
summary(dom_sp_nb)
exp(coef(dom_sp_nb))

AIC(dom_sp_nb)

ndata_spline_nb_dom <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(dom_sp_nb,
                                                                               list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                               se.fit = TRUE)[1:2]),
                                                             c('fit_link','se_link')))

ilink_sp_nb <- family(dom_sp_nb)$linkinv

## create the interval and backtransform
ndata_spline_nb_dom <- mutate(ndata_spline_nb_dom,
                              fit_resp  = ilink_sp_nb(fit_link),
                              right_upr = ilink_sp_nb(fit_link + (2 * se_link)),
                              right_lwr = ilink_sp_nb(fit_link - (2 * se_link)))


########################################################################################## 
### Finally look at all wildlife

combined_animal_cases$all_wild <- combined_animal_cases$jackal + combined_animal_cases$other_wild

# Looks like we probably don't need a spline for the wildlife

lin_wild <- glm.nb(all_wild ~ month_new, data = combined_animal_cases)
summary(lin_wild)
exp(coef(lin_wild))

ndata_lin_wild <- bind_cols(newx, setNames(as_tibble(predict(lin_wild, month_new = newx, se.fit = TRUE)[1:2]),
                                           c('fit_link','se_link')))
## create the interval and backtransform

ilink <- family(lin_wild)$linkinv

ndata_lin_wild <- mutate(ndata_lin_wild,
                         fit_resp  = ilink(fit_link),
                         right_upr = ilink(fit_link + (2 * se_link)),
                         right_lwr = ilink(fit_link - (2 * se_link)))




#### plot for figure

png("Figs/Monthly_regression.png", height=900, width=750)
par(mfrow = c(3,1))
plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "Date",
     ylab = "Number of probable rabies cases", xaxt = "n", cex.axis = 1.6, cex.lab = 1.6)
axis(1, labels = year_labs, at = pos_labs, cex.axis = 1.6)
polygon(c(ndata_spline_nb$month_new, rev(ndata_spline_nb$month_new)), c(ndata_spline_nb$right_upr, rev(ndata_spline_nb$right_lwr)),
        col = "turquoise", lty = 2)
lines(ndata_spline_nb$month_new, ndata_spline_nb$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16)
legend("topright", legend=as.expression(bquote(bold("A  "))), bty = "o", cex = 1.6)


plot(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16, xlab = "Date",
     ylab = "Number of probable rabies cases", xaxt = "n", cex.lab = 1.6, cex.axis = 1.6)
axis(1, labels = year_labs, at = pos_labs, cex.axis = 1.6)
polygon(c(ndata_spline_nb_dom$month_new, rev(ndata_spline_nb_dom$month_new)), c(ndata_spline_nb_dom$right_upr, 
                                                                                rev(ndata_spline_nb_dom$right_lwr)),
        col = "light pink", lty = 2)
lines(ndata_spline_nb_dom$month_new, ndata_spline_nb_dom$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_dom , pch = 16)
legend("topright", legend=as.expression(bquote(bold("B  "))), bty = "o", cex = 1.6)


plot(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16, xlab = "Date",
     ylab = "Number of probable rabies cases", xaxt = "n", cex.axis = 1.6, cex.lab = 1.6)
axis(1, labels = year_labs, at = pos_labs, cex.axis = 1.6)
polygon(c(ndata_lin_wild$month_new, rev(ndata_lin_wild$month_new)), c(ndata_lin_wild$right_upr, 
                                                                      rev(ndata_lin_wild$right_lwr)), 
        col = "light blue", lty = 2)
lines(ndata_lin_wild$month_new, ndata_lin_wild$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16)
legend("topright", legend=as.expression(bquote(bold("C  "))), bty = "o", cex = 1.6)

dev.off()


##### Extra bit to extract the p-value for the coefficients in these models.
# Combined animals

combined_animal_cases$pos <- c(rep(0,80), seq(1,23,1))
combined_animal_cases$neg <- c(seq(-79,0,1), rep(0,23))

slope_mod_combined <- glm.nb(total ~ neg + pos, data = combined_animal_cases)

summary(slope_mod_combined)
exp(coef(slope_mod_combined))

summary(slope_mod_combined)$coefficients # This is all the info. We just need column 2 for SE and 1 for the estimates. 
se_neg_combo <- summary(slope_mod_combined)$coefficients["neg",2]
se_pos_combo <- summary(slope_mod_combined)$coefficients["pos",2]
est_neg <- summary(slope_mod_combined)$coefficients["neg",1]
est_pos <- summary(slope_mod_combined)$coefficients["pos",1]

1-exp(est_neg)
exp(est_pos)
# now use these to calulate the CI on the log scale. 

# initial negative slope
upper_CI_neg_log_combo <- est_neg + (1.96*se_neg_combo)
lower_CI_neg_log_combo <- est_neg - (1.96*se_neg_combo)
exp(upper_CI_neg_log_combo)
exp(lower_CI_neg_log_combo)

1-exp(upper_CI_neg_log_combo)
1-exp(lower_CI_neg_log_combo)


# and positive slope after the spline
upper_CI_pos_log_combo <- est_pos + (1.96*se_pos_combo)
lower_CI_pos_log_combo <- est_pos - (1.96*se_pos_combo)
exp(upper_CI_pos_log_combo)
exp(lower_CI_pos_log_combo)

###########################
#### Domestic animals only 
slope_mod_dom <- glm.nb(all_dom ~ neg + pos, data = combined_animal_cases)

summary(slope_mod_dom)
exp(coef(slope_mod_dom))

summary(slope_mod_dom)$coefficients # This is all the info. We just need column 2 for SE and 1 for the estimates. 
se_neg_dom <- summary(slope_mod_dom)$coefficients["neg",2]
se_pos_dom <- summary(slope_mod_dom)$coefficients["pos",2]
est_neg_dom <- summary(slope_mod_dom)$coefficients["neg",1]
est_pos_dom <- summary(slope_mod_dom)$coefficients["pos",1]


1-exp(est_neg_dom)
1-exp(est_pos_dom)
# now use these to calulate the CI on the log scale. 

# initial negative slope
upper_CI_neg_log_dom <- est_neg_dom + (1.96*se_neg_dom)
lower_CI_neg_log_dom <- est_neg_dom - (1.96*se_neg_dom)
exp(upper_CI_neg_log_dom)
exp(lower_CI_neg_log_dom)

1 - exp(upper_CI_neg_log_dom)
1 - exp(lower_CI_neg_log_dom)

# and positive slope after the spline
upper_CI_pos_log_dom <- est_pos_dom + (1.96*se_pos_dom)
lower_CI_pos_log_dom <- est_pos_dom - (1.96*se_pos_dom)
exp(upper_CI_pos_log_dom)
exp(lower_CI_pos_log_dom)

####### Wildlife - CI around the coefficient of the slope

summary(lin_wild)
exp(coef(lin_wild))
1 - exp(coef(lin_wild))

se_neg_wild <- summary(lin_wild)$coefficients["month_new",2]
est_wild <- summary(lin_wild)$coefficients["month_new",1]

# now use these to calulate the CI on the log scale. 

# initial negative slope
upper_CI_log_wild <- est_wild + (1.96*se_neg_wild)
lower_CI_log_wild <- est_wild - (1.96*se_neg_wild)
exp(upper_CI_log_wild)
exp(lower_CI_log_wild)


1 - exp(upper_CI_log_wild)
1 - exp(lower_CI_log_wild)
