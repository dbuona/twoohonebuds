### Started 11 May 2015 ###
### By Lizzie, at the Whiteley Center ###
### changed from a bud_volume to a bud_volume document by Jehane on May 28, 2015

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

#Make a new column with bud_volume
##Formula for cone volume: V=pi*r^2*(h/3)
dater$bud_volume <- pi*(dater$bud_width/2)^2*(dater$bud_length/3)

#now, in this document, will change bud_width to bud_volume. 

## one way to get summary data
budvolume_summary <- ddply(dater, c("Site", "latbi"), summarise,
    # above: can add bud_location etc. here is you want
    # below: can change to bud_length etc.
    N = length(bud_volume),
    mean = mean(bud_volume),
    sd   = sd(bud_volume),
    se   = sd / sqrt(N)
)

#Note: LYOLIG and VACMYR have "NA" in the summary sheet-- there is a missing measurement for LYOLIG08_HF and VACMYR01_HF. Fix this? 

hfdater <- subset(dater, Site=="Harvard Forest")
shdater <- subset(dater, Site=="Saint Hippolyte")

(sppnotatSH <- unique(hfdater$latbi)[which(!  unique(hfdater$latbi) %in% unique(shdater$latbi)   )]    )

(sppnotatHF <- unique(shdater$latbi)[which(!unique(shdater$latbi)
    %in% unique(hfdater$latbi))]) 
    
daterbothsites <- dater[which(!dater$latbi %in% sppnotatSH),]
daterbothsites <- daterbothsites[which(!daterbothsites$latbi %in% sppnotatHF),]

#names(daterbothsites)
#list(unique(daterbothsites$latbi))


## first let's just look at the data
colorz <- c("firebrick3", "dodgerblue3")
pch <- c(1, 2)

##
## data through time

pdf(file="buds/graphs/budvolumes.pdf", 10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(dater$latbi))){ # sp=1
  subber <- subset(dater, latbi==unique(dater$latbi)[sp])
  plot(bud_volume~date_measure1, data=subber, type="n",
       main=unique(dater$latbi)[sp])
  points(bud_volume~date_measure1, data=subset(subber, Site=="Harvard Forest" 
      & bud_location=="Lateral"), pch=pch[1], col=colorz[1])
  points(bud_volume~date_measure1, data=subset(subber, Site=="Harvard Forest" 
      & bud_location=="Terminal"), pch=pch[2], col=colorz[1])
  points(bud_volume~date_measure1, data=subset(subber, Site=="Saint Hippolyte" 
      & bud_location=="Lateral"), pch=pch[1], col=colorz[2])
  points(bud_volume~date_measure1, data=subset(subber, Site=="Saint Hippolyte" 
      & bud_location=="Terminal"), pch=pch[2], col=colorz[2])

	if(sp == 1) legend("bottom", pch = rep(pch, 2), col = rep(colorz, each=2), legend = c("HF Lat", "HF Term", "SH Lat", "SH Term"))

}
dev.off()
system('open buds/graphs/budvolumes.pdf -a /Applications/Preview.app') # preview can update pdfs without closing

##
## histograms

data <- daterbothsites
columnname <- "bud_volume"
pdf(file=paste("buds/graphs/budvolumes_histograms.pdf", sep=""),
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
system('open buds/graphs/budvolumes_histograms.pdf -a /Applications/Preview.app') 


pdf(file=paste("buds/graphs/budvolumeswsqrt_histograms.pdf", sep=""),
    10, 6, paper="a4r", onefile=TRUE)
for (sp in seq_along(unique(data[["latbi"]]))){
  subber <- subset(data, latbi==unique(data[["latbi"]])[sp])
  par(mfrow=c(2,2))
  hfl <- subset(subber, Site=="Harvard Forest")
  truehist(sqrt(hfl[[columnname]]), xlab=paste(unique(data[["latbi"]])[sp], "at HF"))
  shl <- subset(subber, Site=="Saint Hippolyte")
  truehist(sqrt(shl[[columnname]]), xlab=paste(unique(data[["latbi"]])[sp], "at SH"))
}
dev.off(); system('open buds/graphs/budvolumeswsqrt_histograms.pdf -a /Applications/Preview.app') 

data <- subset(daterbothsites, latbi !="Spiraea_alba")
columnname <- "bud_volume"
pdf(file=paste("buds/graphs/budvolumesloc_histograms.pdf", sep=""),
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
dev.off(); system('open buds/graphs/budvolumesloc_histograms.pdf -a /Applications/Preview.app') 


##
## try some models

# model 1: check for day effect, excluding data measured in March. 
# Logic: exclude the bulk of the SH measurements, now focusing on initial and repeated HF measures, and SH measures made at the repeated measure time (Late April)

daternoMarch <- subset(dater, doy<60 | doy>90)
daternoMarchHF <- subset(daternoMarch, Site=="Harvard Forest")

modelnoMarch <- lm(bud_volume ~ latbi * doy, data=daternoMarch, na.action=na.exclude)
anova(modelnoMarch)
coef(modelnoMarch)
specieslist <- unique(daternoMarchHF$latbi)
listhere <- list()
for (sp in seq_along(specieslist)){
    dataonesp <- subset(daternoMarchHF, latbi==specieslist[sp])
    modelnoMarch <- lm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
    listhere[[paste(sp, specieslist[sp])]] <- list(coef(modelnoMarch), anova(modelnoMarch)) # adding species name and coefs for doy effect
  }

listhere # look at results

# check my work
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
modelwdoy <- lm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
summary(modelwdoy)
dataonesp <- subset(daterbothsites, latbi==specieslist[1])
modelwSite <- lm(bud_volume~Site, data=dataonesp, na.action=na.exclude)
summary(modelwSite)

##
## consider another species: acesac
# there's a weak, small effect of doy (0.0045, so if measured 45 days apart that could make the buds 0.2 bigger)
# there's an effect of site (0.43 +/- 0.06)
# so, the doy effect could mean the real effect is 0.43-0.21 or 0.22
dataonespHF <- subset(daternoMarchHF, latbi==specieslist[1])
modelwdoy <- lm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
summary(modelwdoy)
dataonesp <- subset(daterbothsites, latbi==specieslist[1])
modelwSite <- lm(bud_volume~Site, data=dataonesp, na.action=na.exclude)
summary(modelwSite)


## other models
daterbothsitesnoeff <- daterbothsites[which(daterbothsites$latbi %in% specieslist[noeff]),]
modelnodoy <- lm(bud_volume~latbi*Site, data=daterbothsitesnoeff, na.action=na.exclude)
anova(modelnodoy)

model <- lm(bud_volume~latbi*Site*doy, data=daterbothsites, na.action=na.exclude)
anova(model)
me.model <- lme(bud_volume~Site*doy, random = ~ 1|latbi, data=daterbothsites, na.action=na.exclude)
summary(me.model)
ranef(me.model)

#checking with lmer
library(arm)
me.model2 <- lmer(bud_volume~Site*doy + (1|latbi), data=daterbothsites, na.action=na.exclude)
ranef(me.model2) # idendical

# option to look at each species in step through approach
specieslist <- unique(dater$latbi)
sphere <- 1
dataonesp <- subset(dater, latbi==specieslist[sphere])
modelsp <- lm(bud_volume~Site*doy, data=dataonesp, na.action=na.exclude)
modeldoy.sp <- lm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
modelsite.sp <- lm(bud_volume~Site, data=dataonesp, na.action=na.exclude)
summary(modelsp)
 # this model above includes doy and site, and their interaction
 # the interaction is NA if there are measurements from either site for only one day
 # I am not sure it's worth looking at doy*Site given the data, better model may be:
 # lm(bud_volume~Site+doy, data=dataonesp, na.action=na.exclude)
summary(modeldoy.sp)
summary(modelsite.sp)

# acepen: no effect of site or doy, it seems
# acerub: effect of both (both but doy effect varies by site...)
# acesac: effect of both (site effect of 0.55)
# alninc: marginal site and interaction effect 
# faggra: site and interaction effect, site is negative
# ilemuc: marginal site effect using lm(bud_volume~Site+doy, data=dataonesp, na.action=na.exclude)
# prupen: strong doy effect
# querub: no strong effects
# spialb: no strong effects
# viblat: no strong effects

############## looking at bud volume by site and species (old code that Dan helped with)

hist(daterbothsites$bud_volume)

summary(daterbothsites$bud_volume)
daterbothsites[daterbothsites$bud_volume > 100,] # some really huge buds from Vib lan
b2 <- daterbothsites[daterbothsites$bud_volume < 50,] # focusing on leaf buds for now

hist(b2$bud_volume)

m1 <- lm(log(bud_volume) ~ Site * latbi, data = b2)
summary(m1)
anova(m1)

#plot(m1)
plot(log(bud_volume) ~ Site, data = b2)

plot(log(bud_volume) ~ latbi, data = b2)

shsp <- unique(b2[b2$Site=="St. Hippolyte","latbi"])

b3 <- b2[!is.na(match(b2$latbi, shsp)),]
dim(b3)
sort(unique(b3[b3$Site=="Harvard Forest","latbi"]))
sort(unique(b3[b3$Site=="St. Hippolyte","latbi"]))


# now re-do the analysis with just this dataframe b3, which has only the matching species between HF and SH

m1 <- lm(log(bud_volume) ~ Site * latbi, data = b3)
summary(m1)
anova(m1)
plot(m1)

# did we take different size cuttings at the different sites?

m2 <- lm(log(stem_diameter) ~ Site * latbi, data = b3)
summary(m2)
anova(m2)

# yes, some are bigger, some smaller

# now take that into account

m3 <- lm(log(bud_volume) ~ stem_diameter + Site * latbi, data = b3)
summary(m3)
anova(m3) # even after accounting for stem diameter, site, species, and their interaction still highly significant
plot(m3)

#looks like buds from St. Hippolyte are bigger than those from HF. Most pronounced size difference (greater than 1) for: ACERUB, ACESAC, BETALL, LONCAN, PRUPEN, VIBCAS. 





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

# makehistograms(daterbothsites, "bud_volume", "budvolumehistograms")
