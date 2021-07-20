#-----------------------------------------
# Author: Sarah Hayes
# Title: Regression looking at trend in monthly cases
# First part of the code is taken from KL Incidence plots. 
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
human <- read.csv("output/human_bites.csv")


# ANIMAL DATA ------------------------------------------------------------------

# Look at number of cases in each species in each subset
Animal_cases <- as.data.frame(table(animal$Species))
colnames(Animal_cases) <- c("Species", "N_cases")
Animal_cases$percentage <- round((Animal_cases$N_cases/sum(Animal_cases$N_cases)*100), digits=1)

# Now need number of cases per month; Time series
month_breaks <- 0:((12*length(unique(animal$Year)))-5)
rabies_ts = hist(animal$month_n, breaks=month_breaks, plot=TRUE)$counts

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

hist(combined_animal_cases$total, breaks = seq(0,25,1), xlab = "cases per month")

mod_all <- glm(total ~ month_new, data = combined_animal_cases, family = "poisson")
summary(mod_all)
exp(coef(mod_all))

## method taken from "from the bottom of the heap" blog. 
## https://fromthebottomoftheheap.net/2018/12/10/confidence-intervals-for-glms/

ilink <- family(mod_all)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(newx, setNames(as_tibble(predict(mod_all, month_new = newx, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
## show
#ndata


plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata$month_new, rev(ndata$month_new)), c(ndata$right_upr, rev(ndata$right_lwr)), col = "turquoise", lty = 2)
lines(ndata$month_new, ndata$fit_resp, col = "black", lwd = 2)

AIC(mod_all)


## quadratic term in the model

combined_animal_cases$month_new_sq <- combined_animal_cases$month_new^2

mod_quad <- glm(total ~ month_new + month_new_sq, data = combined_animal_cases, family = "poisson")
summary(mod_quad)
exp(coef(mod_quad))

AIC(mod_quad)

newx_sq <- as.data.frame(newx^2)

ndata_sq <- bind_cols(newx, newx_sq, setNames(as_tibble(predict(mod_quad, list(month_new = newx$month_new, month_new_sq = newx_sq$month_new),
                                                                se.fit = TRUE)[1:2]),
                                              c('fit_link','se_link')))
## create the interval and backtransform
ndata_sq <- mutate(ndata_sq,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))
## show

ndata_sq


plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_sq$month_new...1, rev(ndata_sq$month_new...1)), c(ndata_sq$right_upr, rev(ndata_sq$right_lwr)),
        col = "hot pink", lty = 2)
lines(ndata_sq$month_new...1, ndata_sq$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16)

#dev.off()


## Adding a spline term 
## Harder to decide where to have the change point in this case because it is monthly data

combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 79 < 0, 0, combined_animal_cases$month_new - 79) 

mod_spline <- glm(total ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
summary(mod_spline)
exp(coef(mod_spline))

AIC(mod_spline)

newx_spline <- as.data.frame(combined_animal_cases$spline_bit)
colnames(newx_spline) <- "new_spline_bit"

ndata_spline <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(mod_spline, 
                                  list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                se.fit = TRUE)[1:2]),
                                              c('fit_link','se_link')))

ilink_sp <- family(mod_spline)$linkinv

## create the interval and backtransform
ndata_spline <- mutate(ndata_spline,
                   fit_resp  = ilink_sp(fit_link),
                   right_upr = ilink_sp(fit_link + (2 * se_link)),
                   right_lwr = ilink_sp(fit_link - (2 * se_link)))
## show

#ndata_spline


plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_spline$month_new, rev(ndata_spline$month_new)), c(ndata_spline$right_upr, rev(ndata_spline$right_lwr)),
        col = "light blue", lty = 2)
lines(ndata_spline$month_new, ndata_spline$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16)

## what difference does it make if we move the spline knot position 

combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new -79 < 0, 0, 
                                            combined_animal_cases$month_new - 79) 

mod_spline6 <- glm(total ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
AIC(mod_spline6)


### Spline with 2 knots

combined_animal_cases$spline_bit_2a <- if_else(combined_animal_cases$month_new - 60 < 0, 0, combined_animal_cases$month_new - 60)
combined_animal_cases$spline_bit_2b <- if_else(combined_animal_cases$month_new - 80 < 0, 0, combined_animal_cases$month_new - 80)
 
mod_spline_2bits <- glm(total ~ month_new + spline_bit_2a + spline_bit_2b, data = combined_animal_cases, family = "poisson")
summary(mod_spline_2bits)
exp(coef(mod_spline_2bits))

AIC(mod_spline_2bits)

newx_spline_2bits_a<- as.data.frame(combined_animal_cases$spline_bit_2a)
colnames(newx_spline_2bits_a) <- "new_spline_bit_2a"

newx_spline_2bits_b<- as.data.frame(combined_animal_cases$spline_bit_2b)
colnames(newx_spline_2bits_b) <- "new_spline_bit_2b"

ndata_spline_2bits <- bind_cols(newx, newx_spline_2bits_a, newx_spline_2bits_b,
                                setNames(as_tibble(predict(mod_spline_2bits,
                                list(month_new = newx$month_new, spline_bit_2a = newx_spline_2bits_a$new_spline_bit_2a,
                                     spline_bit_2b = newx_spline_2bits_b$new_spline_bit_2b),
                                                                        se.fit = TRUE)[1:2]),
                                                      c('fit_link','se_link')))

ilink_s2p <- family(mod_spline_2bits)$linkinv

## create the interval and backtransform
ndata_spline_2bits <- mutate(ndata_spline_2bits,
                       fit_resp  = ilink_s2p(fit_link),
                       right_upr = ilink_s2p(fit_link + (2 * se_link)),
                       right_lwr = ilink_s2p(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_spline_2bits$month_new, rev(ndata_spline_2bits$month_new)), 
        c(ndata_spline_2bits$right_upr, rev(ndata_spline_2bits$right_lwr)),
        col = "orange", lty = 2)
lines(ndata_spline_2bits$month_new, ndata_spline_2bits$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16)

## what difference does it make if we move the spline knot position 

combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new -79 < 0, 0, 
                                            combined_animal_cases$month_new - 79) 

mod_spline6 <- glm(total ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
AIC(mod_spline6)


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

## create the interval and backtransform
ndata_spline_nb <- mutate(ndata_spline_nb,
                       fit_resp  = ilink_sp_nb(fit_link),
                       right_upr = ilink_sp_nb(fit_link + (2 * se_link)),
                       right_lwr = ilink_sp_nb(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_spline_nb$month_new, rev(ndata_spline_nb$month_new)), c(ndata_spline_nb$right_upr, rev(ndata_spline_nb$right_lwr)),
        col = "hot pink", lty = 2)
lines(ndata_spline_nb$month_new, ndata_spline_nb$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$total, pch = 16)



#### Looking at AIC with different knot positions
combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 80 < 0, 0, 
                                            combined_animal_cases$month_new - 80) 

mod_sp_nb_2 <- glm.nb(total ~ month_new + spline_bit, data = combined_animal_cases)
#summary(mod_sp_nb_2)
AIC(mod_sp_nb_2)

# 80 is the best cut off value for the negative binomial model. 


##############################################################################################################
###   Repeat for the separate species.
###   Start with domestic animals.

combined_animal_cases$all_dom <- combined_animal_cases$dog + combined_animal_cases$other_dom
hist(combined_animal_cases$all_dom)
mean(combined_animal_cases$all_dom)
var(combined_animal_cases$all_dom)



combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 85 < 0, 0, combined_animal_cases$month_new - 79) 

dom_spline <- glm(all_dom ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
summary(dom_spline)
exp(coef(dom_spline))

AIC(dom_spline)

ndata_spline_dom <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(dom_spline, 
                                                                        list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                        se.fit = TRUE)[1:2]),
                                                      c('fit_link','se_link')))

ilink_dom <- family(dom_spline)$linkinv

## create the interval and backtransform
ndata_spline_dom <- mutate(ndata_spline_dom,
                       fit_resp  = ilink_dom(fit_link),
                       right_upr = ilink_dom(fit_link + (2 * se_link)),
                       right_lwr = ilink_dom(fit_link - (2 * se_link)))


plot(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16, xlab = "month",
     ylab = "number of domestic animal cases")
polygon(c(ndata_spline_dom$month_new, rev(ndata_spline_dom$month_new)), c(ndata_spline_dom$right_upr, rev(ndata_spline_dom$right_lwr)),
        col = "yellow", lty = 2)
lines(ndata_spline_dom$month_new, ndata_spline_dom$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16)

## what difference does it make if we move the spline knot position 

# combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 79 < 0, 0, 
#                                             combined_animal_cases$month_new - 79) 
# 
# mod_spline6 <- glm(all_dom ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
# AIC(mod_spline6)
# 
# AIC(dom_spline)

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

plot(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_spline_nb_dom$month_new, rev(ndata_spline_nb_dom$month_new)), c(ndata_spline_nb_dom$right_upr, 
                                                                                rev(ndata_spline_nb_dom$right_lwr)),
        col = "light pink", lty = 2)
lines(ndata_spline_nb_dom$month_new, ndata_spline_nb_dom$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_dom , pch = 16)




#### Looking at AIC with different knot positions
# combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 80< 0, 0, 
#                                             combined_animal_cases$month_new - 80) 
# 
# mod_sp_nb_2 <- glm.nb(all_dom ~ month_new + spline_bit, data = combined_animal_cases)
# #summary(mod_sp_nb_2)
# AIC(mod_sp_nb_2)

#### Two knots

combined_animal_cases$spline_bit_2a <- if_else(combined_animal_cases$month_new - 60 < 0, 0, combined_animal_cases$month_new - 60)
combined_animal_cases$spline_bit_2b <- if_else(combined_animal_cases$month_new - 80 < 0, 0, combined_animal_cases$month_new - 80)

mod_spline_2bits_dom <- glm.nb(all_dom ~ month_new + spline_bit_2a + spline_bit_2b, data = combined_animal_cases)
summary(mod_spline_2bits_dom)
exp(coef(mod_spline_2bits_dom))

AIC(mod_spline_2bits_dom)

ndata_spline_2bits_dom <- bind_cols(newx, newx_spline_2bits_a, newx_spline_2bits_b,
                                setNames(as_tibble(predict(mod_spline_2bits_dom,
                                                           list(month_new = newx$month_new, spline_bit_2a = newx_spline_2bits_a$new_spline_bit_2a,
                                                                spline_bit_2b = newx_spline_2bits_b$new_spline_bit_2b),
                                                           se.fit = TRUE)[1:2]),
                                         c('fit_link','se_link')))

# All have the same inverse link function so no need to generate every time
## create the interval and backtransform
ndata_spline_2bits_dom <- mutate(ndata_spline_2bits_dom,
                             fit_resp  = ilink_s2p(fit_link),
                             right_upr = ilink_s2p(fit_link + (2 * se_link)),
                             right_lwr = ilink_s2p(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16, xlab = "month",
     ylab = "number of animal cases")
polygon(c(ndata_spline_2bits_dom$month_new, rev(ndata_spline_2bits_dom$month_new)), 
        c(ndata_spline_2bits_dom$right_upr, rev(ndata_spline_2bits_dom$right_lwr)),
        col = "violet", lty = 2)
lines(ndata_spline_2bits_dom$month_new, ndata_spline_2bits_dom$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_dom, pch = 16)

########################################################################################## 
### Finally look at all wildlife

combined_animal_cases$all_wild <- combined_animal_cases$jackal + combined_animal_cases$other_wild
hist(combined_animal_cases$all_wild)
mean(combined_animal_cases$all_wild)
var(combined_animal_cases$all_wild)



combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 80 < 0, 0, combined_animal_cases$month_new - 79) 

wild_spline <- glm(all_wild ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
summary(wild_spline)
exp(coef(wild_spline))

AIC(wild_spline)

ndata_spline_wild <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(wild_spline, 
                                                                            list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                            se.fit = TRUE)[1:2]),
                                                          c('fit_link','se_link')))

ilink_wild <- family(wild_spline)$linkinv

## create the interval and backtransform
ndata_spline_wild <- mutate(ndata_spline_wild,
                           fit_resp  = ilink_wild(fit_link),
                           right_upr = ilink_wild(fit_link + (2 * se_link)),
                           right_lwr = ilink_wild(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16, xlab = "month",
     ylab = "number of wild animal cases")
polygon(c(ndata_spline_wild$month_new, rev(ndata_spline_wild$month_new)), c(ndata_spline_wild$right_upr, rev(ndata_spline_wild$right_lwr)),
        col = "salmon", lty = 2)
lines(ndata_spline_wild$month_new, ndata_spline_wild$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16)

## what difference does it make if we move the spline knot position 

# combined_animal_cases$spline_bit <- if_else(combined_animal_cases$month_new - 60 < 0, 0, 
#                                             combined_animal_cases$month_new - 60) 
# 
# mod_spline_w <- glm(all_wild ~ month_new + spline_bit, data = combined_animal_cases, family = "poisson")
# AIC(mod_spline_w)
# summary(mod_spline_w)
# AIC(dom_spline)

##### Negative binomial model

wild_sp_nb <- glm.nb(all_wild ~ month_new + spline_bit, data = combined_animal_cases)
summary(wild_sp_nb)
exp(coef(wild_sp_nb))

AIC(wild_sp_nb)

ndata_spline_nb_wild <- bind_cols(newx, newx_spline, setNames(as_tibble(predict(wild_sp_nb,
                                                                               list(month_new = newx$month_new, spline_bit = newx_spline$new_spline_bit),
                                                                               se.fit = TRUE)[1:2]),
                                                             c('fit_link','se_link')))

ilink_sp_nb <- family(wild_sp_nb)$linkinv

## create the interval and backtransform
ndata_spline_nb_wild <- mutate(ndata_spline_nb_wild,
                              fit_resp  = ilink_sp_nb(fit_link),
                              right_upr = ilink_sp_nb(fit_link + (2 * se_link)),
                              right_lwr = ilink_sp_nb(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16, xlab = "month",
     ylab = "number of wild animal cases")
polygon(c(ndata_spline_nb_wild$month_new, rev(ndata_spline_nb_wild$month_new)), c(ndata_spline_nb_wild$right_upr, 
                                                                                rev(ndata_spline_nb_wild$right_lwr)),
        col = "deepskyblue", lty = 2)
lines(ndata_spline_nb_wild$month_new, ndata_spline_nb_wild$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_wild , pch = 16)



##### 
# Looks like we probably don't need a spline for the wildlife

lin_wild <- glm.nb(all_wild ~ month_new, data = combined_animal_cases)
summary(lin_wild)
exp(coef(lin_wild))
AIC(lin_wild)

ndata_lin_wild <- bind_cols(newx, setNames(as_tibble(predict(lin_wild, month_new = newx, se.fit = TRUE)[1:2]),
                                  c('fit_link','se_link')))
## create the interval and backtransform
ndata_lin_wild <- mutate(ndata_lin_wild,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))

plot(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16, xlab = "month",
     ylab = "number of wild animal cases")
polygon(c(ndata_lin_wild$month_new, rev(ndata_lin_wild$month_new)), c(ndata_lin_wild$right_upr, 
                        rev(ndata_lin_wild$right_lwr)), col = "springgreen1", lty = 2)
lines(ndata_lin_wild$month_new, ndata_lin_wild$fit_resp, col = "black", lwd = 2)
points(combined_animal_cases$month_new, combined_animal_cases$all_wild, pch = 16)

AIC(lin_wild)




