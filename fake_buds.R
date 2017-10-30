###fake data for buds 19 Oct 2017

rm(list=ls())
options(stringsAsFactors = FALSE)
graphics.off()
setwd("~/Documents/git/twoohonebuds")

library(ggplot2)
library(dplyr)
library(plyr)
library(lme4)
library(rstanarm)
library(rstan)
library(arm)
library(msm)

#5 specue species fake data for tru_bud vol
set.seed(73)


##################################################################
## This version has only simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!
##################################################################

# Note to self (Lizzie): could improve code, so easier to see distribution for a and sigma_y
# I did this below #

# nlab = 10 # number of labgroups
nsp = 5 # number of species

ntot = 50 # numbers of obs per species.


#  with species  (note to self: This is not the best, better to draw from a distribution)
baseinter <- 5 # baseline intercept (days to BB) across all species
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

# now start building ...
testdat <- vector()


for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  doy<- rtnorm(ntot,40,10,lower=10,upper=70) 
  
  ## set up effect size
  doycoef<-0.5
  doycoef.sd<-0.05
  
  # build model matrix 
  mm <- model.matrix(~doy, data.frame(doy))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(spint[i], 
             rnorm(1, doycoef, doycoef.sd))

  
  bvol <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(bvol, sp = i, 
                         doy)
  
  testdat <- rbind(testdat, testdatx)  
}


truvol<-stan_lmer(bvol~doy+(1|sp), testdat)
print(truvol)
pp_check(truvol)
ranef(truvol)


###make adjusted bvol
testdat<-dplyr::mutate(testdat, offset=40-doy)
testdat<-dplyr::mutate(testdat, deltabv= offset*0.5)
testdat<-dplyr::mutate(testdat, trubudvol= bvol-deltabv)

### fill in new column


  


