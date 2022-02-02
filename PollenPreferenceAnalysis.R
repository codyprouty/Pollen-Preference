##

#Analysis for pollen preference manuscript

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
Change <- subset(Weight, Change > -1)
names(Pollen)[1] <- "Cage"
###

#Effect of treatment on percent change in pollen
WeightSum <- aggregate(AdjChange~Cage + Treatment, data=Weight, FUN=sum)

Change <- mixed(AdjChange ~ Treatment + (1|Cage), data = WeightSum, method="LRT")
nice(Change)

lsm<-lsmeans (CM, list( ~ Treatment))
cld(lsm)
###

#Effect of treatment on activity observations
ActSum <- aggregate(TimeSpent~ Cage+Treatment, data = Activity, FUN=sum)
ActSum$TimeSpent.t <- ActSum$TimeSpent +1

Obs <- mixed(TimeSpent.t ~ Treatment + (1|Cage), family = gaussian(link = "log"), data = ActSum, method="LRT")
Obs

summary(Obs)
###