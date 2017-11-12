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
library(brms)

#5 specue species fake data for tru_bud vol
set.seed(73)


##################################################################
## This version has only simple linear model with no interactions:
# bb ~ force + photo + chill
# and only random intercepts for species!
##################################################################

##vary intercept


nsp = 5 # number of species
ntot = 50 # numbers of obs per species.

baseinter <- 5 # baseline intercept (budvol) across all species
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

# now start building ...
testdat <- vector()


for(i in 1:nsp){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  doy<- rtnorm(ntot,40,10,lower=10,upper=70) 
  
  ## set up effect size
  doycoef<-.5
  doycoef.sd<-0.05
  
  # build model matrix 
  mm <- model.matrix(~doy, data.frame(doy))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  coeff <- c(spint[i], 
             rnorm(1, doycoef, doycoef.sd))

  bvol <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(bvol, sp = i, 
                         doy)
  testdat <- rbind(testdat, testdatx)  
}


truvol<-lmer(bvol~doy+(1|sp), testdat)

summary(truvol)


###This doesnt seem to work for my hierarchical model, only when tru vol is lm)
b<-ranef(truvol)
c<-40
vol<-testdat$bvol

for(i in 1:nsp){
testdat$tru<-vol-b[2]*(vol-c)
}

#alternative ###make adjusted bvol
testdat<-dplyr::mutate(testdat, offset=40-doy)
testdat<-dplyr::mutate(testdat, deltabv= offset*0.52)
testdat<-dplyr::mutate(testdat, trubudvol= bvol-deltabv)

######################################################################start here
#### fake data for doy effect varying slope
nsp = 5 # number of species
ntot = 100 # numbers of obs per species.
baseinter <- 5 # baseline intercept (budvol) across all species
spint <- c(6,4,3,8,5.5) # different intercepts by species
baseeff<-2 ##baseline effect size
speff<- c(.5,-.1,.8,.3,.6) ##diferent effect by species
# now start building ...
testdat2 <- vector()
for(i in 1:nsp){ # loop over species. i = 1
  # continuous predictors, generate level for each observation
  doy<- rtnorm(ntot,30,10,lower=20,upper=90) 
  ## set up effect size
  doycoef<-speff
  doycoef.sd<-.5
  # build model matrix 
  mm <- model.matrix(~doy, data.frame(doy))
  # coefficients need to match the order of the colums in the model matrix (mm)
  coeff <- c(spint[i], 
             rnorm(1, doycoef[i], doycoef.sd))
  
  bvol <- rtnorm(n = ntot, mean = mm %*% coeff, sd = 6,lower=1, upper=11)
  
  testdatx <- data.frame(bvol, sp = i, 
                         doy)
  testdat2 <- rbind(testdat2, testdatx)  
}

ggplot(testdat2,aes(bvol))+geom_density()+facet_wrap(~sp)

###model (try this in lmer cause it takes for ever)

truvol.slope<-lmer(bvol~doy+(doy|sp), testdat2)
###This seems to work
summary(truvol.slope)
fixef(truvol.slope)
pred<-predict(truvol.slope)
plot(pred,testdat2$bvol)
plot(truvol.slope)
plot(testdat2$bvol)

### Below adjusts to "true" bud vol at day
slopes<-coef(truvol.slope)
B<-slopes$sp$doy
c<-40
vol<-testdat2$bvol
for(i in 1:nsp){
  testdat2$tru<-testdat2$bvol-(testdat2$bvol-40)*B[i]
}

