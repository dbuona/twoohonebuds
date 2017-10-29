###fake data for buds 19 Oct 2017

For onspecies
a <- 7.2
DOY <- 2.1
Site <- -5
s <- 3
y <- rnorm( length(x) , mean=a + DOY*x + Site*z , sd=s )






######## This copys ospreee generate but Im not sure I want to do it that way

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

nsp<-10 ##number of species
nrep<-20 ###buds/sp
ntot<-nsp*nrep

intermean <- 7 # mean for budvol
intersd <- 4 # SD for selecting species intercepts
spint <- rnorm(nsp, intermean, intersd)

###build
testdat <- vector()

for(i in 1:nsp){ # loop over species. i = 1
  
  # binomail predictors for each observation
  DOY = rbinom(ntot, 2, .3)
  site = rbinom(ntot, 1, .5)
  
  # set up effect sizes
  DOYcoef = 2  
  sitecoef = 1 
 
  # SD for each treatment
  DOYcoef.sd = 0.2
  sitecoef.sd = 0.2 

  # build model matrix 
  mm <- model.matrix(~DOY+site, data.frame(DOY, site))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  coeff <- c(spint[i], 
             rnorm(1, DOYcoef, DOYcoef.sd),
             rnorm(1, sitecoef, sitecoef.sd)
  )
  
  bvol <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(bvol, sp = i, 
                         DOY, site)
  
  testdat <- rbind(testdat, testdatx)
}
