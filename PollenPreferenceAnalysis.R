##

#Analysis for lithium chloride manuscript

#Contact: cprouty@ufl.edu    https://scholar.google.com/citations?user=PpeDx78AAAAJ&hl=en

##

#load packages
library(lme4)
library(lsmeans)
library(multcomp)
library(afex)
##

#Import files
Activity <- read.csv("Activity.csv")
Weight <- read.csv("WeightChange.csv")
Pollen <- read.csv("PollenPreference.csv")
##

#Data organization
names(Activity)[1] <- "Cage"
Activity$Cage <- as.factor(Activity$Cage)
Activity$Day <- as.factor(Activity$Day)
Activity$Observation <- as.factor(Activity$Observation)
names(Weight)[1] <- "Cage"
Weight$Cage <- as.factor(Weight$Cage)
Weight$Day <- as.factor(Weight$Day)
names(Pollen)[1] <- "Cage"
###

#Effect of treatment on percent change in pollen
Change <- lmer(Change ~ Treatment + (1|Cage) + (1|Day), data = Change)
anova(Change)

lsm<-lsmeans (Change, list( ~ Treatment))
cld(lsm)
###

#Effect of treatment on activity observations
Obs <- mixed(TotalObs ~ Treatment + (1|Cage), family = poisson (link = "log"), data = Pollen, method="LRT")
Obs

lsm<-lsmeans (Obs, list( ~ Treatment))
cld(lsm)
###