### Read back in tables with significant coefficients ####

setwd("~/Documents/Intrasp.perform.land/intra.models/New.Results")

pseuin=read.csv("pseuin.output.csv", header=T)
parach=read.csv("parach.output.csv", header=T)
pittke=read.csv("pittke.output.csv", header = T)

#### Make dotcharts of coefficients ####
# Plots made in R instead of Rstudio

#### pseuin ####

param.labels=c("LMA","RMF",
               "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2","RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1","LMA x RMF x Soil.PC2")

dotchart(pseuin[137:152,2], labels=param.labels, cex=1.5, xlim=c(-0.35, 0.35),
       main = expression(italic("P. indochinensis, n = 373")),
       xlab = "Standardized Coefficients")

abline(v=0, lty=2)
lines(x=c(pseuin[138,4], pseuin[138,5]), y=c(2,2),lwd=2) # rmf
lines(x=c(pseuin[139,4], pseuin[139,5]), y=c(3,3),lwd=2) # initial size
lines(x=c(pseuin[140,4], pseuin[140,5]), y=c(4,4),lwd=2) # light
lines(x=c(pseuin[141,4], pseuin[141,5]), y=c(5,5),lwd=2) # Comp1
lines(x=c(pseuin[145,4], pseuin[145,5]), y=c(9,9),lwd=2) # lma*Comp1
lines(x=c(pseuin[147,4], pseuin[147,5]), y=c(11,11),lwd=2) # rmf*light

points(x=pseuin[138,2], y=2, pch=19)
points(x=pseuin[139,2], y=3, pch=19)
points(x=pseuin[140,2], y=4, pch=19)
points(x=pseuin[141,2], y=5, pch=19)
points(x=pseuin[145,2], y=9, pch=19)
points(x=pseuin[147,2], y=11, pch=19)

#### PARACH ####

param.labels=c("LMA","RMF",
               "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2","RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1","LMA x RMF x Soil.PC2")

dotchart(parach[106:121,2], labels=param.labels, cex = 1.5,
         xlim=c(-0.45,0.45), main = expression(italic("P. chinensis, n = 194")),
         xlab = "Standardized Coefficients")

abline(v=0, lty=2)
lines(x=c(parach[107,4], parach[107,5]), y=c(2,2),lwd=2) #rmf
lines(x=c(parach[108,4], parach[108,5]), y=c(3,3),lwd=2) #initial site
lines(x=c(parach[109,4], parach[109,5]), y=c(4,4),lwd=2) #light
lines(x=c(parach[110,4], parach[110,5]), y=c(5,5),lwd=2) #Comp1
lines(x=c(parach[112,4], parach[112,5]), y=c(7,7),lwd=2) #lma*rmf
lines(x=c(parach[121,4], parach[121,5]), y=c(16,16),lwd=2) #lma*rmf*Comp2

points(x=parach[107,2], y=2, pch=19)
points(x=parach[108,2], y=3, pch=19)
points(x=parach[109,2], y=4, pch=19)
points(x=parach[110,2], y=5, pch=19)
points(x=parach[112,2], y=7, pch=19)
points(x=parach[121,2], y=16, pch=19)

#### PITTKE ####
param.labels=c("LMA","RMF",
               "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2","RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1","LMA x RMF x Soil.PC2")

dotchart(pittke[79:94,2], labels=param.labels, cex=1.5, xlim=c(-0.5,0.5),
         main = expression(italic("P. kerrii, n = 108")),
         xlab = "Standardized Coefficients")

abline(v=0, lty=2)
lines(x=c(pittke[85,4], pittke[85,5]), y=c(7,7),lwd=2) # lma*rmf
lines(x=c(pittke[93,4], pittke[93,5]), y=c(15,15),lwd=2) # lma*rmf*Comp1

points(x=pittke[85,2], y=7, pch=19)
points(x=pittke[93,2], y=15, pch=19)

