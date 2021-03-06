###modified Jehanes code of Sept 20 (buds_volume.R)

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
library("rmutil")

dater<-read.csv("input/BUDSET_Dissection_Data_April30.csv")
colnames(dater)
####cleaning
unique(dater$species)
unique(dater$Genus)
##### spaces in genus;fix
dater$Genus <- sub("^ | $", "", dater$Genus) 
####make name
dater$name<-paste(dater$Genus,dater$species,sep="_")
unique(dater$name)
###returns Populus grandifolia (not a species) Fix
dater$species[dater$Genus=="Populus" & dater$species=="grandifolia"] <- "grandidentata"
dater$name<-paste(dater$Genus,dater$species,sep="_")
unique(dater$name)

#clean date
dater$date_measure1 <- as.Date(dater$date_measure, format="%Y-%m-%d")
dater$doy <- as.numeric(format(dater$date_measure1, "%j"))
unique(dater$Site)
###clean Saint Hippolyte
dater$Site[dater$Site=="St. Hippolyte"] <- "Saint Hippolyte"

###clean location: 
dater$bud_location <- sub("^ | $", "", dater$bud_location) 
unique(dater$bud_location)
dater$bud_location[dater$bud_location=="Terminal_twig"] <- "Terminal"
dater$bud_location[dater$bud_location=="Pseudoterminal_twig"] <- "Pseudoterminal" ###to I wantt o just name this terminal
unique(dater$bud_location)

####calculate bud volume: Formula for cone volume: V=pi*r^2*(h/3)
dater$bud_volume <- pi*(dater$bud_width/2)^2*(dater$bud_length/3)

dater$nickname[dater$name=="Acer_pensylvanicum"] <- "A.pe"
dater$nickname[dater$name=="Acer_rubrum"] <- "A.ru"
dater$nickname[dater$name=="Acer_saccharum"] <- "A.sa"
dater$nickname[dater$name=="Alnus_incana"] <- "A.in"
dater$nickname[dater$name=="Betula_alleghaniensis" ] <- "B.al"
dater$nickname[dater$name=="Betula_papyrifera" ] <- "B.pa"
dater$nickname[dater$name=="Corylus_cornuta" ] <- "C.co"
dater$nickname[dater$name=="Fagus_grandifolia" ] <- "F.gr"
dater$nickname[dater$name=="Fraxinus_nigra" ] <- "F.ni"
dater$nickname[dater$name=="Ilex_mucronata" ] <- "I.mu"
dater$nickname[dater$name=="Lonicera_canadensis" ] <- "L.ca"
dater$nickname[dater$name=="Populus_grandidentata" ] <- "P.gr"
dater$nickname[dater$name=="Prunus_pensylvanica" ] <- "P.pe"
dater$nickname[dater$name=="Quercus_rubra" ] <- "Q.ru"
dater$nickname[dater$name=="Spiraea_alba" ] <- "S.al"
dater$nickname[dater$name=="Vaccinium_myrtilloides" ] <- "V.my"
dater$nickname[dater$name=="Viburnum_cassinoides" ] <- "V.ca"
dater$nickname[dater$name=="Viburnum_lantanoides" ] <- "V.la"


###explore:
budvolume_summary <- ddply(dater, c("Site", "name"), summarise,
# above: can add bud_location etc. here is you want
# below: can change to bud_length etc.
N = length(bud_volume),
mean = mean(bud_volume),
sd   = sd(bud_volume),
se   = sd / sqrt(N))

### what are the species that overlap between the two sites?
hfdater <- subset(dater, Site=="Harvard Forest")
shdater <- subset(dater, Site=="Saint Hippolyte")
(sppnotatSH <- unique(hfdater$name)[which(!  unique(hfdater$name) %in% unique(shdater$name)   )]    )
(sppnotatHF <- unique(shdater$name)[which(!unique(shdater$name) %in% unique(hfdater$name))]) 
daterbothsites <- dater[which(!dater$name %in% sppnotatSH),]
daterbothsites <- daterbothsites[which(!daterbothsites$name %in% sppnotatHF),]
###name shorter names


#####Plottting

ggplot(daterbothsites,aes(nickname,bud_volume,col=Site))+stat_summary()
### but we know ther is bias in the date of measuring
ggplot(daterbothsites, aes(doy))+geom_histogram(binwidth=1,aes(color=Site))

###in general buds grow. how do we correct for this??

### does location matter for size
ggplot(daterbothsites,aes(nickname,bud_volume,col=bud_location))+stat_summary()

###look over time to see bias
# model 1: check for day effect, excluding data measured in March. 
# Logic: exclude the bulk of the SH measurements, now focusing on initial and repeated HF measures, and SH measures made at the repeated measure time (Late April)

###note i am doing this with bud_volume instead of width as jehane did
daternoMarch <- subset(dater, doy<60 | doy>90)
daternoMarchHF <- subset(daternoMarch, Site=="Harvard Forest")

modelnoMarch <- lm(bud_volume ~ name * doy, data=daternoMarch, na.action=na.exclude)
summary(modelnoMarch)
coef(modelnoMarch)
anova(modelnoMarch)


specieslist <- unique(daternoMarchHF$name)
listhere <- list()
#for (sp in seq_along(specieslist)){
  dataonesp <- subset(daternoMarchHF, name==specieslist[sp])
  modelnoMarch <- stan_glm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
  pp_check(modelnoMarch)
  listhere[[paste(sp, specieslist[sp])]] <- list(coef(modelnoMarch)) # adding species name and coefs for doy effect
}
listhere

dataonespHF <- subset(daternoMarchHF, name==specieslist[1])
modelwdoy <- lm(bud_volume~doy, data=dataonesp, na.action=na.exclude)
summary(modelwdoy)
dataonesp <- subset(daterbothsites, name==specieslist[1])

modelwSite <- lm(bud_volume~Site, data=dataonesp, na.action=na.exclude)
summary(modelwSite)



#####Dan's attempt to bias correct and model "true bud volume
#true bud volume ~  a[sp/ind] + doy[sp] + measured bud volume[sp]

## basic true bud volume with comeplete pooling all data
ggplot(daternoMarch,aes(bud_volume))+geom_density()
ggplot(daternoMarch,aes(bud_volume))+geom_density()+facet_wrap(~nickname)
###try centering
daternoMarch$log_bvol<-log(daternoMarch$bud_volume)
ggplot(daternoMarch,aes(log_bvol))+geom_density()

truvol<-stan_lmer(log_bvol~doy+(doy|name), daternoMarch)

print(truvol)
pp_check(truvol)
launch_shinystan(truvol)


#bias correct
display(truvol)
coef(truvol)
ranef(truvol)
predict(truvol)



#################################part II########################## devising a response variable########################
## can i use Dan F's data as a response

d<-read.csv("input/Budburst By Day.csv")

dd<-dplyr::select(d,ind,rep,lday,bday)
meanbb<- dplyr::group_by(dd,ind)
meanbb<-dplyr::summarise(meanbb,meanbday=mean(bday))

colnames(dater)[which(names(dater) == "individual_ID")] <- "ind"
gooby<- dplyr::group_by(dater,ind)
gooby<-dplyr::summarise(gooby,meanbvol=mean(bud_volume))

newdat<-right_join(meanbb,gooby,by="ind")

newdat<-separate(newdat,ind, c("ID", "site"), "_")
newdat$ind<-paste(newdat$ID,newdat$site,sep="_")
blug<-left_join(newdat,dater, by="ind")
blug<-dplyr::select(blug, ID.x,site,meanbday,meanbvol,ind,name)
blug<-blug[!duplicated(blug),]

####problem: pseduoreplication i think implies each bud on the indiviual burst on the same day
###we dont actually know which bud burst I don't think


####Models

mod1<-lm(meanbday~meanbvol,data=newdat)
summary(mod1)
mod2<-lmer(meanbday~meanbvol+(1+name|site),data=blug)
summary(mod2)
coef(mod2)
##
