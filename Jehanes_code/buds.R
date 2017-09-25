### Started 11 May 2015 ###
### By Lizzie, at the Whiteley Center ###

## First look at the bud data! ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages used below
library(MASS)
library(plyr)
library(nlme)

# DF: setwd("/Users/danflynn/Documents/git")
<<<<<<< Updated upstream
# Lab computer: setwd("~/GitHub")
=======
#JS: setwd("/Users/jsamaha/Documents/Bud Set Project")
>>>>>>> Stashed changes
setwd("/Users/Lizzie/Documents/git/projects/treegarden/analyses/")

dater <- read.csv("buds/input/BUDSET_Dissection_Data_April30.csv", header=TRUE)

## some data clean up
#dater$Genus[dater$Genus=="Alnus "] <- "Alnus"
#dater$Genus[dater$Genus=="Betula "] <- "Betula"
dater$Genus <- sub("^ | $", "", dater$Genus) # Alternative for cleaning up whitespace

# Fix Popgra spelling; was grandifolia in SH
dater$species[dater$Genus=="Populus" & dater$species=="grandifolia"] <- "grandidentata"

dater$latbi <- paste(dater$Genus, dater$species, sep="_")
# unique(dater$latbi)

dater$date_measure1 <- as.Date(dater$date_measure, format="%Y-%m-%d")
#dater$date_measure1 <- as.Date(dater$date_measure, format="%d-%b-%Y")
    # see http://www.statmethods.net/input/dates.html
dater$doy <- as.numeric(format(dater$date_measure1, "%j")) # day of year
dater$Site[dater$Site=="St. Hippolyte"] <- "Saint Hippolyte"
dater <- subset(dater, latbi!="_")
# make fewer locations categories
dater$bud_location[dater$bud_location=="Terminal "] <- "Terminal"
dater$bud_location[dater$bud_location=="Terminal_twig"] <- "Terminal"
dater$bud_location[dater$bud_location=="Pseudoterminal"] <- "Terminal"
dater$bud_location[dater$bud_location=="Pseudoterminal "] <- "Terminal"
dater$bud_location[dater$bud_location=="Pseudoterminal_twig"] <- "Terminal"

#unique(dater$bud_location)
# Alternative to clean up leading trailing whitespace, using regular expression: 
dater$bud_location <- sub("^ | +$", "", dater$bud_location)

#dater$bud_location[dater$bud_location=="Lateral "] <- "Lateral"
#dater$bud_location[dater$bud_location==" Lateral"] <- "Lateral"

## one way to get summary data
budwidth_summary <- ddply(dater, c("Site", "latbi"), summarise,
    # above: can add bud_location etc. here is you want
    # below: can change to bud_length etc.
    N = length(bud_width),
    mean = mean(bud_width),
    sd   = sd(bud_width),
    se   = sd / sqrt(N)
)

hfdater <- subset(dater, Site=="Harvard Forest")
shdater <- subset(dater, Site=="Saint Hippolyte")

(sppnotatSH <- unique(hfdater$latbi)[which(!  unique(hfdater$latbi) %in% unique(shdater$latbi)   )]    )

(sppnotatHF <- unique(shdater$latbi)[which(!unique(shdater$latbi)
    %in% unique(hfdater$latbi))]) 
    
daterbothsites <- dater[which(!dater$latbi %in% sppnotatSH),]
daterbothsites <- daterbothsites[which(!daterbothsites$latbi %in% sppnotatHF),]


## first let's just look at the data
colorz <- c("firebrick3", "dodgerblue3")
pch <- c(1, 2)

##
## data through time

pdf(file="buds/graphs/budwidths.pdf", 10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(dater$latbi))){ # sp=1
  subber <- subset(dater, latbi==unique(dater$latbi)[sp])
  plot(bud_width~date_measure1, data=subber, type="n",
       main=unique(dater$latbi)[sp])
  points(bud_width~date_measure1, data=subset(subber, Site=="Harvard Forest" 
      & bud_location=="Lateral"), pch=pch[1], col=colorz[1])
  points(bud_width~date_measure1, data=subset(subber, Site=="Harvard Forest" 
      & bud_location=="Terminal"), pch=pch[2], col=colorz[1])
  points(bud_width~date_measure1, data=subset(subber, Site=="Saint Hippolyte" 
      & bud_location=="Lateral"), pch=pch[1], col=colorz[2])
  points(bud_width~date_measure1, data=subset(subber, Site=="Saint Hippolyte" 
      & bud_location=="Terminal"), pch=pch[2], col=colorz[2])

	if(sp == 1) legend("bottom", pch = rep(pch, 2), col = rep(colorz, each=2), legend = c("HF Lat", "HF Term", "SH Lat", "SH Term"))

}
dev.off()
system('open buds/graphs/budwidths.pdf -a /Applications/Preview.app') # preview can update pdfs without closing

##
## histograms

data <- daterbothsites
columnname <- "bud_width"
pdf(file=paste("buds/graphs/budwidths_histograms.pdf", sep=""),
    10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(data[["latbi"]]))){
  subber <- subset(data, latbi==unique(data[["latbi"]])[sp])
  par(mfrow=c(2,2))
  hfl <- subset(subber, Site=="Harvard Forest")
  truehist(hfl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at HF"))
  shl <- subset(subber, Site=="Saint Hippolyte")
  truehist(shl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at SH"))
}
dev.off()
system('open buds/graphs/budwidths_histograms.pdf -a /Applications/Preview.app') 


pdf(file=paste("buds/graphs/budwidthswsqrt_histograms.pdf", sep=""),
    10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(data[["latbi"]]))){
  subber <- subset(data, latbi==unique(data[["latbi"]])[sp])
  par(mfrow=c(2,2))
  hfl <- subset(subber, Site=="Harvard Forest")
  truehist(sqrt(hfl[[columnname]]), xlab=paste(unique(data[["latbi"]])[sp], "at HF"))
  shl <- subset(subber, Site=="Saint Hippolyte")
  truehist(sqrt(shl[[columnname]]), xlab=paste(unique(data[["latbi"]])[sp], "at SH"))
}
dev.off(); system('open buds/graphs/budwidthswsqrt_histograms.pdf -a /Applications/Preview.app') 

data <- subset(daterbothsites, latbi !="Spiraea_alba")
columnname <- "bud_width"
pdf(file=paste("buds/graphs/budwidthsloc_histograms.pdf", sep=""),
    10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(data[["latbi"]]))){
  subber <- subset(data, latbi==unique(data[["latbi"]])[sp])
  par(mfrow=c(2,2))
  hfl <- subset(subber, Site=="Harvard Forest" & bud_location=="Lateral")
  truehist(hfl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at HF, lateral"))
  hft <- subset(subber, Site=="Harvard Forest" & bud_location=="Terminal")
  truehist(hft[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at HF, terminal"))
  shl <- subset(subber, Site=="Saint Hippolyte" & bud_location=="Lateral")
  truehist(shl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at SH, lateral"))
  sht <- subset(subber, Site=="Saint Hippolyte" & bud_location=="Terminal")
  truehist(sht[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at SH, terminal"))
    } # breaks at Vib lan
dev.off(); system('open buds/graphs/budwidthsloc_histograms.pdf -a /Applications/Preview.app') 


##
## try some models

# model 1: check for day effect, excluding data measured in March. 
# Logic: exclude the bulk of the SH measurements, now focusing on initial and repeated HF measures, and SH measures made at the repeated measure time (Late April)

daternoMarch <- subset(dater, doy<60 | doy>90)
daternoMarchHF <- subset(daternoMarch, Site=="Harvard Forest")

modelnoMarch <- lm(bud_width ~ latbi * doy, data=daternoMarch, na.action=na.exclude)
anova(modelnoMarch)
coef(modelnoMarch)
specieslist <- unique(daternoMarchHF$latbi)
listhere <- list()
for (sp in seq_along(specieslist)){
    dataonesp <- subset(daternoMarchHF, latbi==specieslist[sp])
    modelnoMarch <- lm(bud_width~doy, data=dataonesp, na.action=na.exclude)
    listhere[[paste(sp, specieslist[sp])]] <- list(coef(modelnoMarch), anova(modelnoMarch)) # adding species name and coefs for doy effect
  }

listhere # look at results

# check my work (JS: checked! all good).
weaksig <- c(1, 3, 14, 22, 25)
strongsig <- c(2, 5, 7, 17, 18, 19)
	# acerub: strong positive
	# popgra: strong negative and at both sites! Only one twig at SH. 
	# prupen: strong positive and at both sites.
   # some are negative, e.g., aromel, betlen, popgra, quealb
   # bias is okay if in OPPOSITE direction of site effect
   # but all of these are HF only spp I think
noeff <- c(4, 6, 8, 9, 10:13, 15:16, 20, 21, 23, 24, 26, 27)

specieslist[weaksig]
specieslist[strongsig]
specieslist[noeff]


##
## consider one species: acepen
# there's a weak, small effect of doy (0.0015, so if measured 60 days apart that could make the buds 0.0015*60 bigger) # DF: I see it as only 0.008
# there's no real effect of site and it's negative (-0.21)
# so, maybe there is a doy effect but it's not impacting site
dataonespHF <- subset(daternoMarchHF, latbi==specieslist[1])
modelwdoy <- lm(bud_width~doy, data=dataonesp, na.action=na.exclude)
summary(modelwdoy)
dataonesp <- subset(daterbothsites, latbi==specieslist[1])
modelwSite <- lm(bud_width~Site, data=dataonesp, na.action=na.exclude)
summary(modelwSite)

##
## consider another species: acesac
# there's a weak, small effect of doy (0.0045, so if measured 45 days apart that could make the buds 0.2 bigger)
# there's an effect of site (0.43 +/- 0.06)
# so, the doy effect could mean the real effect is 0.43-0.21 or 0.22
dataonespHF <- subset(daternoMarchHF, latbi==specieslist[1])
modelwdoy <- lm(bud_width~doy, data=dataonesp, na.action=na.exclude)
summary(modelwdoy)
dataonesp <- subset(daterbothsites, latbi==specieslist[1])
modelwSite <- lm(bud_width~Site, data=dataonesp, na.action=na.exclude)
summary(modelwSite)


## other models
daterbothsitesnoeff <- daterbothsites[which(daterbothsites$latbi %in% specieslist[noeff]),]
modelnodoy <- lm(bud_width~latbi*Site, data=daterbothsitesnoeff, na.action=na.exclude)
anova(modelnodoy)

model <- lm(bud_width~latbi*Site*doy, data=daterbothsites, na.action=na.exclude)
anova(model)
me.model <- lme(bud_width~Site*doy, random = ~ 1|latbi, data=daterbothsites, na.action=na.exclude)
summary(me.model)
ranef(me.model)

#checking with lmer
library(arm)
me.model2 <- lmer(bud_width~Site*doy + (1|latbi), data=daterbothsites, na.action=na.exclude)
ranef(me.model2) # idendical

# option to look at each species in step through approach
specieslist <- unique(dater$latbi)
sphere <- 1
dataonesp <- subset(dater, latbi==specieslist[sphere])
modelsp <- lm(bud_width~Site*doy, data=dataonesp, na.action=na.exclude)
modeldoy.sp <- lm(bud_width~doy, data=dataonesp, na.action=na.exclude)
modelsite.sp <- lm(bud_width~Site, data=dataonesp, na.action=na.exclude)
summary(modelsp)
 # this model above includes doy and site, and their interaction
 # the interaction is NA if there are measurements from either site for only one day
 # I am not sure it's worth looking at doy*Site given the data, better model may be:
 # lm(bud_width~Site+doy, data=dataonesp, na.action=na.exclude)
summary(modeldoy.sp)
summary(modelsite.sp)

# acepen: no effect of site or doy, it seems
# acerub: effect of both (both but doy effect varies by site...)
# acesac: effect of both (site effect of 0.55)
# alninc: marginal site and interaction effect 
# faggra: site and interaction effect, site is negative
# ilemuc: marginal site effect using lm(bud_width~Site+doy, data=dataonesp, na.action=na.exclude)
# prupen: strong doy effect
# querub: no strong effects
# spialb: no strong effects
# viblat: no strong effects







## non-working function below
## 
makehistograms <- function(data, columnname, filename){
  # requires a latbi column
  # requires a dataframe where all species have terminal and lateral buds
  # and only works for species at present at both sites
pdf(file=paste("graphs/", filename, ".pdf", sep=""),
    10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(data[["latbi"]]))){
  subber <- subset(data, latbi==unique(data[["latbi"]])[sp])
  par(mfrow=c(2,2))
  hfl <- subset(subber, Site=="Harvard Forest" & bud_location=="Lateral")
  truehist(hfl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at HF, lateral"))
  hft <- subset(subber, Site=="Harvard Forest" & bud_location=="Terminal")
  truehist(hft[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at HF, terminal"))
  shl <- subset(subber, Site=="Saint Hippolyte" & bud_location=="Lateral")
  truehist(shl[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at SH, lateral"))
  sht <- subset(subber, Site=="Saint Hippolyte" & bud_location=="Terminal")
  truehist(sht[[columnname]], xlab=paste(unique(data[["latbi"]])[sp], "at SH, terminal"))
    }
dev.off()
}
# makehistograms(daterbothsites, "bud_width", "budwidthhistograms")
