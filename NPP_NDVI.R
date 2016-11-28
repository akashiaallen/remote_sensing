rm(list=ls())
#set working directory to relevant folder under Session and then name the file
vegin <- read.csv("NPP_VIs_2003.csv", header=T)
#to see the names of the columns
names(vegin)

attach(vegin)
#plot NPP against each of the Veg indices
plot(MeanANPP, MeanEVI, pch=16, col="blue")
plot(MeanANPP, MeanNDVI, pch=16, col="purple")
plot(MeanANPP, MeanSAVI, pch=16, col="darkGreen")
plot(MeanANPP, MeanMSAVI, pch=16, col="red")

#look at each site separately
plot(MeanANPP, MeanEVI, pch=16, col=as.numeric(site))
plot(MeanANPP, MeanNDVI, pch=16, col=as.numeric(site))
plot(MeanANPP, MeanSAVI, pch=16, col=as.numeric(site))
plot(MeanANPP, MeanMSAVI, pch=16, col=as.numeric(site))
#legend for sites
#legend("topright", legend = as.numeric(site), names(site), col=c("blue", "black", "green"), title="Sites")


#Pearson's correlation between NPP and each veg index
cor.test(MeanANPP, MeanEVI)
cor.test(MeanANPP, MeanNDVI)
cor.test(MeanANPP, MeanSAVI)
cor.test(MeanANPP, MeanMSAVI)

detach(vegin)

#open new file
avhrr <- read.csv("AVHRR_NPP_NDVI.csv", header=T)
names(avhrr)

attach(avhrr)

#plot NPP against four NDVI variables, amp, dur, maxn, totnd
plot(Avg_ANPP, AMP, pch=16, col="blue")
#Pearson's correlataion between NPP and four NDVI variables
cor.test(Avg_ANPP, AMP)
#linear regression of each of four variables with NPP
lm(AMP~Avg_ANPP)
#draw best fit line
abline(lm(AMP~Avg_ANPP), col="orange")
#details of the line
summary(lm(AMP~Avg_ANPP))

plot(Avg_ANPP, DUR, pch=16, col="purple")
cor.test(Avg_ANPP, DUR)
lm(DUR~Avg_ANPP)
abline(lm(DUR~Avg_ANPP), col="orange")
summary(lm(DUR~Avg_ANPP))

plot(Avg_ANPP, MAXN, pch=16, col="darkGreen")
cor.test(Avg_ANPP, MAXN)
lm(MAXN~Avg_ANPP)
abline(lm(MAXN~Avg_ANPP), col="orange")
summary(lm(MAXN~Avg_ANPP))

plot(Avg_ANPP, TOTND, pch=16, col="red")
cor.test(Avg_ANPP, TOTND)
lm(TOTND~Avg_ANPP)
abline(lm(TOTND~Avg_ANPP), col="orange")
summary(lm(TOTND~Avg_ANPP))

#look at each site separately
plot(Avg_ANPP, AMP, pch=16, col=as.numeric(site))
plot(Avg_ANPP, DUR, pch=16, col=as.numeric(site))
plot(Avg_ANPP, MAXN, pch=16, col=as.numeric(site))
plot(Avg_ANPP, TOTND, pch=16, col=as.numeric(site))

detach(avhrr)
##############################################
#multiple linear regression using climate data
nppamp <- read.csv("NPP_AMP.csv", header=T)
attach(nppamp)
#scatterplots of all possible pairs in data set
pairs(nppamp, panel=panel.smooth)
#create tree model to view significant parameters
install.packages("tree")
par(mfrow=c(1,1))
library(tree)
modela <- tree(AMP~.,data=nppamp)
plot(modela)
text(modela)

#significant parameters from tree model chosen for linear regression
modela1 <- lm(AMP~Avg_ANPP+AnnualPrecip+Monsoon+Events+Ev_Size+Extreme_size+CDD+Extreme_CDD)
summary(modela1)

#try step function to help simplify model
modela2 <- step(modela1, direction="backward")
summary(modela2)

#plot model to see significance
plot(modela2)

#compare tree variables to final model

detach(nppamp)
###########################################
#continue process for DUR, MAXN, and TOTND
nppdur <- read.csv("NPP_DUR.csv", header=T)
attach(nppdur)
#scatterplots of all possible pairs in data set
pairs(nppdur, panel=panel.smooth)
#create tree model to view significant parameters
par(mfrow=c(1,1))

modeld <- tree(DUR~.,data=nppdur)
plot(modeld)
text(modeld)

#significant parameters from tree model chosen for linear regression
modeld1 <- lm(DUR~Avg_ANPP+AnnualPrecip+Monsoon+Events+Ev_Size+Extreme_size+CDD+Extreme_CDD)
summary(modeld1)

#try step function to help simplify model
modeld2 <- step(modeld1, direction="backward")
summary(modeld2)

#plot model to see significance
plot(modeld2)

#compare tree variables to final model

detach(nppdur)

#############################################
nppmaxn <- read.csv("NPP_MAXN.csv", header=T)
attach(nppmaxn)
#scatterplots of all possible pairs in data set
pairs(nppmaxn, panel=panel.smooth)
#create tree model to view significant parameters
par(mfrow=c(1,1))

modelm <- tree(MAXN~.,data=nppmaxn)
plot(modelm)
text(modelm)

#significant parameters from tree model chosen for linear regression
modelm1 <- lm(MAXN~Avg_ANPP+AnnualPrecip+Monsoon+Events+Ev_Size+Extreme_size+CDD+Extreme_CDD)
summary(modelm1)

#try step function to help simplify model
modelm2 <- step(modelm1, direction="backward")
summary(modelm2)

#plot model to see significance
plot(modelm2)

#compare tree variables to final model

detach(nppmaxn)

################################################
npptotnd <- read.csv("NPP_TOTND.csv", header=T)
attach(npptotnd)
#scatterplots of all possible pairs in data set
pairs(npptotnd, panel=panel.smooth)
#create tree model to view significant parameters
par(mfrow=c(1,1))

modelt <- tree(TOTND~.,data=npptotnd)
plot(modelt)
text(modelt)

#significant parameters from tree model chosen for linear regression
modelt1 <- lm(TOTND~Avg_ANPP+AnnualPrecip+Monsoon+Events+Ev_Size+Extreme_size+CDD+Extreme_CDD)
summary(modelt1)

#try step function to help simplify model
modelt2 <- step(modelt1, direction="backward")
summary(modelt2)

#plot model to see significance
plot(modelt2)

#compare tree variables to final model

detach(npptotnd)

