setwd("~/Documents/Peterson_Data/")
getwd()

Kipukalm <- read.csv("AppendixS4.csv", header=TRUE)[,-1]
head(Kipukalm)

#head(Kipukalm[-2,])
#remove outlier 
#lm1 <- lm(formula = areaHA ~ comp, data = Kipukalm[-2,])
#print(lm1)
#summary(lm1)
#w/outlier scatter.smooth(x=Kipukalm$areaHA, y=Kipukalm$comp, main="Comp ~ areaHA") 
#scatter.smooth(x=Kipukalm[-2,]$areaHA, y=Kipukalm[-2,]$comp, main="Comp ~ areaHA", col="dimgray", pch=16)

#lm2 <- lm(formula = distanceout ~ comp, data = Kipukalm[-4,])
#head(Kipukalm[-4,])
#print(lm2)
#summary(lm2)
#scatter.smooth(x=Kipukalm[-4,]$distanceout, y=Kipukalm[-4,]$comp, main="Comp ~ distanceout", col="dimgray", pch=16) 


lm3 <- lm(formula = comp ~ elevationm, data = Kipukalm)
print(lm3)
summary(lm3)
scatter.smooth(x=Kipukalm$elevationm, y=Kipukalm$comp, main="Comp ~ elevationm") 
plot(Kipukalm$elevationm, Kipukalm$comp, main="Comp ~ elevationm", col="dimgray", pch=16)
abline(lm(Kipukalm$comp ~ Kipukalm$elevationm))

lm4 <- lm(formula = comp ~ spprichness, data = Kipukalm)
print(lm4)
summary(lm4)
scatter.smooth(x=Kipukalm$spprichness, y=Kipukalm$comp, main="Comp ~ spprichness",col = "dimgray", pch=16) 
plot(Kipukalm$spprichness, Kipukalm$comp, main="Comp ~ spprichness", col="dimgray", pch=16)
abline(lm(Kipukalm$comp ~ Kipukalm$spprichness))

## Test for interactions
lm3_1 <- lm(formula = comp ~ elevationm * areaHA, data = Kipukalm)
print(lm3_1)
summary(lm3_1)

lm3_2 <- lm(formula = comp ~ elevationm * distanceout, data = Kipukalm)
print(lm3_2)
summary(lm3_2)

lm3_3 <- lm(formula = comp ~ elevationm *spprichness , data = Kipukalm)
print(lm3_3)
summary(lm3_3)

## stil just spprichness is significant
lm3_4 <- lm(formula = comp ~ areaHA *spprichness , data = Kipukalm)
print(lm3_4)
summary(lm3_4)

lm3_5 <- lm(formula = comp ~ areaHA *distanceout , data = Kipukalm)
print(lm3_5)
summary(lm3_5)

lm3_6 <- lm(formula = comp ~ spprichness *distanceout , data = Kipukalm)
print(lm3_6)
summary(lm3_6)


#lm5 <- lm(formula = areaHA ~ filt, data = Kipukalm)
#print(lm5)
#summary(lm5)
#scatter.smooth(x=Kipukalm[-2,]$areaHA, y=Kipukalm[-2,]$filt, main="Filt ~ areaHA", col = "dimgray", pch=16) 
#plot(Kipukalm$filt, Kipukalm$areaHA, main="Filt ~ areaHA", col="dimgray", pch=16)
#abline(lm(Kipukalm$areaHA ~ Kipukalm$filt))

#lm6 <- lm(formula = distanceout ~ filt, data = Kipukalm[-4,])
#print(lm6)
#summary(lm6)
#scatter.smooth(x=Kipukalm[-4,]$distanceout, y=Kipukalm[-4,]$filt, main="Filt ~ distanceout", pch = 16, col= "dimgray") 

lm7 <- lm(formula = filt ~ elevationm, data = Kipukalm)
print(lm7)
summary(lm7)
scatter.smooth(x=Kipukalm$elevationm, y=Kipukalm$filt, main="Filt ~ elevationm", pch=16, col ="dimgray") 
plot(Kipukalm$elevationm, Kipukalm$filt, main="Filt ~ elevationm", col="dimgray", pch=16)
abline(lm(Kipukalm$filt ~ Kipukalm$elevationm))

lm8 <- lm(formula = filt ~ spprichness, data = Kipukalm)
print(lm8)
summary(lm8)
#scatter.smooth(x=Kipukalm$spprichness, y=Kipukalm$filt, main="Filt ~ spprichness") 
plot(Kipukalm$spprichness, Kipukalm$filt, main="Filt ~ spprichness", col="dimgray", pch=16)
abline(lm(Kipukalm$filt ~ Kipukalm$spprichness))

## Test for interactions
lm7_1 <- lm(formula = filt ~ elevationm * areaHA, data = Kipukalm)
print(lm7_1)
summary(lm7_1)

## elevation already signifcant
lm7_2 <- lm(formula = filt ~ elevationm * distanceout, data = Kipukalm)
print(lm7_2)
summary(lm7_2)

lm7_3 <- lm(formula = filt ~ elevationm *spprichness , data = Kipukalm)
print(lm7_3)
summary(lm7_3)

## stil just spprichness is significant
lm7_4 <- lm(formula = filt ~ areaHA *spprichness , data = Kipukalm)
print(lm7_4)
summary(lm7_4)

lm7_5 <- lm(formula = filt ~ areaHA *distanceout , data = Kipukalm)
print(lm7_5)
summary(lm7_5)

lm7_6 <- lm(formula = filt ~ spprichness *distanceout , data = Kipukalm)
print(lm7_6)
summary(lm7_6)


#lm9 <- lm(formula = areaHA ~ neut, data = Kipukalm)
#print(lm9)
#summary(lm9)
#scatter.smooth(x=Kipukalm[-2,]$areaHA, y=Kipukalm[-2,]$neut, main="Neut ~ areaHA", pch=16, col="dimgray") 
#plot(Kipukalm$elevationm, Kipukalm$filt, main="Filt ~ elevationm", col="dimgray", pch=16)
#abline(lm(Kipukalm$elevationm ~ Kipukalm$filt))

#lm10 <- lm(formula = distanceout ~ neut, data = Kipukalm[-4,])
#print(lm10)
#summary(lm10)
#scatter.smooth(x=Kipukalm[-4,]$distanceout, y=Kipukalm[-4,]$neut, main="Neut ~ distanceout", pch=16, col="dimgray") 

lm11 <- lm(formula = neut ~ elevationm, data = Kipukalm)
print(lm11)
summary(lm11)
scatter.smooth(x=Kipukalm$elevationm, y=Kipukalm$neut, main="Neut ~ elevationm",col = "dimgray", pch=16) 
plot(Kipukalm$elevationm, Kipukalm$neut, main="Neut ~ elevationm", col="dimgray", pch=16)
abline(lm(Kipukalm$neut ~ Kipukalm$elevationm))

lm12 <- lm(formula = neut ~ spprichness, data = Kipukalm)
print(lm12)
summary(lm12)
scatter.smooth(x=Kipukalm$spprichness, y=Kipukalm$neut, main="Neut ~ spprichness",col = "dimgray", pch=16) 
plot(Kipukalm$spprichness, Kipukalm$neut, main="Neut ~ spprichness", col="dimgray", pch=16)
abline(lm(Kipukalm$neut ~ Kipukalm$spprichness))

## Test for interactions
lm7_1 <- lm(formula = neut ~ elevationm * areaHA, data = Kipukalm)
print(lm7_1)
summary(lm7_1)

lm7_2 <- lm(formula = neut ~ elevationm * distanceout, data = Kipukalm)
print(lm7_2)
summary(lm7_2)

lm7_3 <- lm(formula = neut ~ elevationm *spprichness , data = Kipukalm)
print(lm7_3)
summary(lm7_3)

lm7_4 <- lm(formula = neut ~ areaHA *spprichness , data = Kipukalm)
print(lm7_4)
summary(lm7_4)

lm7_5 <- lm(formula = neut ~ areaHA *distanceout , data = Kipukalm)
print(lm7_5)
summary(lm7_5)

lm7_6 <- lm(formula = neut ~ spprichness *distanceout , data = Kipukalm)
print(lm7_6)
summary(lm7_6)

#-------------
# Multiple Linear Regression Example 
fitcomp <- lm(Kipukalm$comp ~ areaHA + distanceout + elevationm + spprichness, data=Kipukalm)
summary(fitcomp) # show results

fitcomp <- lm(Kipukalm$comp ~ areaHA + distanceout + spprichness, data=Kipukalm)
summary(fitcomp) # show results

fitcomp <- lm(Kipukalm$comp ~ areaHA + distanceout, data=Kipukalm)
summary(fitcomp) # show results

fitcomp <- lm(Kipukalm$comp ~ areaHA + spprichness, data=Kipukalm)
summary(fitcomp) # show results

fitcomp <- lm(Kipukalm$comp ~ distanceout + spprichness, data=Kipukalm)
summary(fitcomp) # show results

fitcomp <- lm(Kipukalm$comp ~ elevationm + spprichness, data=Kipukalm)
summary(fitcomp) # show results
#spp richness has effect on model support of filtering, but model only explains ~20% of variance in model support

fitfilt <- lm(Kipukalm$filt ~ areaHA + distanceout + elevationm + spprichness, data=Kipukalm)
summary(fitfilt) # show results
#elevation has an effect on support for filtering, but model only explains ~20% of variance in model support

fitfilt <- lm(Kipukalm$filt ~ areaHA + spprichness, data=Kipukalm)
summary(fitfilt) # show results

fitneut <- lm(Kipukalm$neut ~ areaHA + distanceout + elevationm + spprichness, data=Kipukalm)
summary(fitneut)
#only spp richness has an effect on support for neutral, but model only explains ~32% of variance in model support
