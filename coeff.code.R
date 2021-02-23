### Read back in tables with significant coefficients

setwd("~/Desktop/Intrasp.perform.land/intra.models/Coeff.tables")

pseuin=read.csv("pseuin.coeff.table.csv", header=T)
parach=read.csv("parach.coeff.table.csv", header=T)
pittke=read.csv("pittke.coeff.table.csv", header = T)
saprte=read.csv("saprte.coeff.table.csv", header=T)
walsyu=read.csv("walsyu.coeff.table.csv", header=T)
amoodu=read.csv("amoodu.coeff.table.csv", header=T)
dichge=read.csv("dichge.coeff.table.csv", header=T)

### Make dotcharts of coefficients
# Plots made in R instead of Rstudio

param.labels=c("LMA","RMF", "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", "Initial.Size","Light",expression(bold("Soil.PC1")), "Soil.PC2",
               expression(bold("*LMA x SMF")), "LMA x RMF","LMA x Light", expression(bold("*LMA x Soil.PC1")),
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", "SSL x Soil.PC2",
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")

## pseuin
dotchart(pseuin[137:174,2], labels=param.labels, cex=1, xlim=c(-3.4, 1.8),
       main = expression(italic("P. indochinensis, n = 373")),
       xlab = "Standardized Coefficients")

abline(v=0, lty=2)
lines(x=c(pseuin[146,4], pseuin[146,5]), y=c(10,10),lwd=2)
lines(x=c(pseuin[148,4], pseuin[148,5]), y=c(12,12),lwd=2)
lines(x=c(pseuin[151,4], pseuin[151,5]), y=c(15,15),lwd=2)

points(x=pseuin[146,2], y=10, pch=19)
points(x=pseuin[148,2], y=12, pch=19)
points(x=pseuin[151,2], y=15, pch=19)

param.labels.sig=c("Soil.PC1","*LMA x SMF", "*LMA x Soil.PC1")

## pseuin
dotchart(pseuin[c(146,148,151),2], labels=param.labels.sig, cex=1, xlim=c(-0.2, 0.25),
         main = expression(italic("P. indochinensis, n = 373")),
         xlab = "Standardized Coefficients")

abline(v=0, lty=2)
lines(x=c(pseuin[146,4], pseuin[146,5]), y=c(1,1),lwd=2)
lines(x=c(pseuin[148,4], pseuin[148,5]), y=c(2,2),lwd=2)
lines(x=c(pseuin[151,4], pseuin[151,5]), y=c(3,3),lwd=2)

points(x=pseuin[146,2], y=1, pch=19)
points(x=pseuin[148,2], y=2, pch=19)
points(x=pseuin[151,2], y=3, pch=19)

## PARACH
param.labels=c("LMA","RMF", "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", expression(bold("Initial.Size")),expression(bold("Light")),expression(bold("Soil.PC1")), "Soil.PC2",
               "LMA x SMF", "LMA x RMF","LMA x Light", expression(bold("*LMA x Soil.PC1")),
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", expression(bold("*RMF x Soil.PC1")),
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", expression(bold("*SSL x Soil.PC2")),
               expression(bold("*Leaf.Thickness x Soil.PC1")), "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", expression(bold("*LMA x RMF x Soil.PC2")),
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")
dotchart(parach[106:143,2], labels=param.labels, cex = 1,
         xlim=c(-3.4,1.8), main = expression(italic("P. chinensis, n = 194")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(parach[113,4], parach[113,5]), y=c(8,8),lwd=2)
lines(x=c(parach[114,4], parach[114,5]), y=c(9,9),lwd=2)
lines(x=c(parach[115,4], parach[115,5]), y=c(10,10),lwd=2)
lines(x=c(parach[120,4], parach[120,5]), y=c(15,15),lwd=2)
lines(x=c(parach[129,4], parach[129,5]), y=c(24,24),lwd=2)
lines(x=c(parach[132,4], parach[132,5]), y=c(27,27),lwd=2)
lines(x=c(parach[133,4], parach[133,5]), y=c(28,28),lwd=2)
lines(x=c(parach[141,4], parach[141,5]), y=c(36,36),lwd=2)
points(x=parach[113,2], y=8, pch=19)
points(x=parach[114,2], y=9, pch=19)
points(x=parach[115,2], y=10, pch=19)
points(x=parach[120,2], y=15, pch=19)
points(x=parach[129,2], y=24, pch=19)
points(x=parach[132,2], y=27, pch=19)
points(x=parach[133,2], y=28, pch=19)
points(x=parach[141,2], y=36, pch=19)


param.labels.sig=c("Initial.Size","Light","Soil.PC1", "*LMA x Soil.PC1",
                "*RMF x Soil.PC1", "*SSL x Soil.PC2",
               "*Leaf.Thickness x Soil.PC1", "*LMA x RMF x Soil.PC2")

dotchart(parach[c(113,114,115,120,129,132,133,141),2], labels=param.labels.sig, cex = 1,
         xlim=c(-0.6,0.5), main = expression(italic("P. chinensis, n = 194")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(parach[113,4], parach[113,5]), y=c(1,1),lwd=2)
lines(x=c(parach[114,4], parach[114,5]), y=c(2,2),lwd=2)
lines(x=c(parach[115,4], parach[115,5]), y=c(3,3),lwd=2)
lines(x=c(parach[120,4], parach[120,5]), y=c(4,4),lwd=2)
lines(x=c(parach[129,4], parach[129,5]), y=c(5,5),lwd=2)
lines(x=c(parach[132,4], parach[132,5]), y=c(6,6),lwd=2)
lines(x=c(parach[133,4], parach[133,5]), y=c(7,7),lwd=2)
lines(x=c(parach[141,4], parach[141,5]), y=c(8,8),lwd=2)
points(x=parach[113,2], y=1, pch=19)
points(x=parach[114,2], y=2, pch=19)
points(x=parach[115,2], y=3, pch=19)
points(x=parach[120,2], y=4, pch=19)
points(x=parach[129,2], y=5, pch=19)
points(x=parach[132,2], y=6, pch=19)
points(x=parach[133,2], y=7, pch=19)
points(x=parach[141,2], y=8, pch=19)

## WALSYU
param.labels=c("LMA",expression(bold("RMF")), "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x SMF", "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2", expression(bold("*LMF x LAR")), "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", "SSL x Soil.PC2",
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", expression(bold("*LMA x SMF x Light")), "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")
dotchart(walsyu[33:70,2], labels=param.labels, cex=1,
         xlim=c(-3.4,1.8), main = expression(italic("W. yunnanensis, n = 80")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(walsyu[34,4], walsyu[34,5]), y=c(2,2),lwd=2)
lines(x=c(walsyu[49,4], walsyu[49,5]), y=c(17,17),lwd=2)
lines(x=c(walsyu[63,4], walsyu[63,5]), y=c(31,31),lwd=2)
points(x=walsyu[34,2], y=2, pch=19)
points(x=walsyu[49,2], y=17, pch=19)
points(x=walsyu[63,2], y=31, pch=19)

param.labels.sig=c("RMF", "*LMF x LAR", "*LMA x SMF x Light")

dotchart(walsyu[c(34,49,63),2], labels=param.labels.sig, cex=1,
         xlim=c(-3.4,1.8), main = expression(italic("W. yunnanensis, n = 80")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(walsyu[34,4], walsyu[34,5]), y=c(1,1),lwd=2)
lines(x=c(walsyu[49,4], walsyu[49,5]), y=c(2,2),lwd=2)
lines(x=c(walsyu[63,4], walsyu[63,5]), y=c(3,3),lwd=2)
points(x=walsyu[34,2], y=1, pch=19)
points(x=walsyu[49,2], y=2, pch=19)
points(x=walsyu[63,2], y=3, pch=19)


## PITTKE
param.labels=c("LMA","RMF", "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x SMF", expression(bold("*LMA x RMF")),"LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", "SSL x Soil.PC2",
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               expression(bold("*RMF x Leaf.Thickness x Soil.PC1")), "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")
dotchart(pittke[79:116,2], labels=param.labels, cex=1,
         xlim=c(-3.4,1.8), main = expression(italic("P. kerrii, n = 108")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(pittke[91,4], pittke[91,5]), y=c(13,13),lwd=2)
lines(x=c(pittke[113,4], pittke[113,5]), y=c(35,35),lwd=2)
points(x=pittke[91,2], y=13, pch=19)
points(x=pittke[113,2], y=35, pch=19)

param.labels.sig=c("*LMA x RMF","*RMF x Leaf.Thickness x Soil.PC1")

dotchart(pittke[c(91,113),2], labels=param.labels.sig, cex=1,
         xlim=c(-1,0.1), main = expression(italic("P. kerrii, n = 108")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(pittke[91,4], pittke[91,5]), y=c(1,1),lwd=2)
lines(x=c(pittke[113,4], pittke[113,5]), y=c(2,2),lwd=2)
points(x=pittke[91,2], y=1, pch=19)
points(x=pittke[113,2], y=2, pch=19)


## SAPRTE
param.labels=c("LMA","RMF", "Leaf.Thickness",expression(bold("SSL")),"LAR","LMF",
               "SMF", "Initial.Size","Light",expression(bold("Soil.PC1")), expression(bold("Soil.PC2")),
               "LMA x SMF", "LMA x RMF",expression(bold("LMA x Light")), "LMA x Soil.PC1",
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", expression(bold("SMF x Light")), "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", expression(bold("*RMF x Soil.PC1")),
               expression(bold("*RMF x Soil.PC2")), "SSL x Leaf.Thickness", expression(bold("*SSL x Soil.PC2")),
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               expression(bold("*LMA x RMF x Soil.PC1")), "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")

param.labels.sig=c("SSL","Soil.PC1", "Soil.PC2","LMA x Light","SMF x Light",
                   "*RMF x Soil.PC1","*RMF x Soil.PC2", "*SSL x Soil.PC2",
                   "*LMA x RMF x Soil.PC1")


dotchart(saprte[39:76,2], labels=param.labels, cex=1,
         xlim=c(-3.4,1.8), main = expression(italic("S. ternata, n = 54")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(saprte[42,4], saprte[42,5]), y=c(4,4),lwd=2)
lines(x=c(saprte[48,4], saprte[48,5]), y=c(10,10),lwd=2)
lines(x=c(saprte[49,4], saprte[49,5]), y=c(11,11),lwd=2)
lines(x=c(saprte[52,4], saprte[52,5]), y=c(14,14),lwd=2)
lines(x=c(saprte[58,4], saprte[58,5]), y=c(20,20),lwd=2)
lines(x=c(saprte[62,4], saprte[62,5]), y=c(24,24),lwd=2)
lines(x=c(saprte[63,4], saprte[63,5]), y=c(25,25),lwd=2)
lines(x=c(saprte[65,4], saprte[65,5]), y=c(27,27),lwd=2)
lines(x=c(saprte[71,4], saprte[71,5]), y=c(33,33),lwd=2)
points(x=saprte[42,2], y=4, pch=19)
points(x=saprte[48,2], y=10, pch=19)
points(x=saprte[49,2], y=11, pch=19)
points(x=saprte[52,2], y=14, pch=19)
points(x=saprte[58,2], y=20, pch=19)
points(x=saprte[62,2], y=24, pch=19)
points(x=saprte[63,2], y=25, pch=19)
points(x=saprte[65,2], y=27, pch=19)
points(x=saprte[71,2], y=33, pch=19)

# significant variables

dotchart(saprte[c(42,48,49,52,58,62,63,65,71),2], labels=param.labels.sig, cex=1,
         xlim=c(-3.4,1.8), main = expression(italic("S. ternata, n = 54")),
         xlab = "Standardized Coefficients")
abline(v=0, lty=2)
lines(x=c(saprte[42,4], saprte[42,5]), y=c(1,1),lwd=2)
lines(x=c(saprte[48,4], saprte[48,5]), y=c(2,2),lwd=2)
lines(x=c(saprte[49,4], saprte[49,5]), y=c(3,3),lwd=2)
lines(x=c(saprte[52,4], saprte[52,5]), y=c(4,4),lwd=2)
lines(x=c(saprte[58,4], saprte[58,5]), y=c(5,5),lwd=2)
lines(x=c(saprte[62,4], saprte[62,5]), y=c(6,6),lwd=2)
lines(x=c(saprte[63,4], saprte[63,5]), y=c(7,7),lwd=2)
lines(x=c(saprte[65,4], saprte[65,5]), y=c(8,8),lwd=2)
lines(x=c(saprte[71,4], saprte[71,5]), y=c(9,9),lwd=2)
points(x=saprte[42,2], y=1, pch=19)
points(x=saprte[48,2], y=2, pch=19)
points(x=saprte[49,2], y=3, pch=19)
points(x=saprte[52,2], y=4, pch=19)
points(x=saprte[58,2], y=5, pch=19)
points(x=saprte[62,2], y=6, pch=19)
points(x=saprte[63,2], y=7, pch=19)
points(x=saprte[65,2], y=8, pch=19)
points(x=saprte[71,2], y=9, pch=19)


## amoodu
param.labels=c("LMA","RMF", "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x SMF", "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", "SSL x Soil.PC2",
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")

dotchart(amoodu[39:76,2], labels=param.labels, cex=1, xlim=c(-3.4, 1.8),
         main = expression(italic("A. duodecimantha, n = 65")),
         xlab = "Standardized Coefficients")

abline(v=0, lty=2)

## dichge
param.labels=c("LMA","RMF", "Leaf.Thickness","SSL","LAR","LMF",
               "SMF", "Initial.Size","Light","Soil.PC1", "Soil.PC2",
               "LMA x SMF", "LMA x RMF","LMA x Light", "LMA x Soil.PC1",
               "LMA x Soil.PC2", "LMF x LAR", "LMF x Soil.PC2",
               "SMF x Leaf.Thickness", "SMF x Light", "SMF x Soil.PC1",
               "RMF x Leaf.Thickness", "RMF x Light", "RMF x Soil.PC1",
               "RMF x Soil.PC2", "SSL x Leaf.Thickness", "SSL x Soil.PC2",
               "Leaf.Thickness x Soil.PC1", "Leaf.Thickness x Soil.PC2",
               "LAR x Soil.PC2", "LMA x SMF x Light", "LMA x RMF x Light",
               "LMA x RMF x Soil.PC1", "SMF x Leaf.Thickness x Soil.PC1",
               "RMF x Leaf.Thickness x Soil.PC1", "LMA x RMF x Soil.PC2",
               "LMF x LAR x Soil.PC2", "SSL x Leaf.Thickness x Soil.PC2")

dotchart(dichge[33:70,2], labels=param.labels, cex=1, xlim=c(-3.4, 1.8),
         main = expression(italic("D. gelonioides, n = 42")),
         xlab = "Standardized Coefficients")

abline(v=0, lty=2)
