## SAPRTE slopes

# Read in observed values

setwd("~/Desktop/Intrasp.perform.land/Outputs")
saprte.obs=read.csv("saprte.csv", header=T, row.names=1)

# Slopes for two-way interaction model

saprte.lma.light=saprte[39,2]+saprte[52,2]*saprte.obs$log_light
saprte.smf.light=saprte[45,2]+saprte[58,2]*saprte.obs$log_light
saprte.rmf.pc1=saprte[40,2]+saprte[62,2]*saprte.obs$Comp1
saprte.rmf.pc2=saprte[40,2]+saprte[63,2]*saprte.obs$Comp2
saprte.ssl.pc2=saprte[42,2]+saprte[65,2]*saprte.obs$Comp2

# Plotting slopes against other trait/environment

plot(saprte.obs$log_light, saprte.lma.light,type="l", xlab="Light",
     ylab="Slope of LMA-RGR Relationship", ylim=c(-2,2), main="SAPRTE")# doesn't cross 0
abline(h=0, lty=2)
plot(saprte.obs$log_light, saprte.smf.light,type="l", xlab="Light",
     ylab="Slope of SMF-RGR Relationship", ylim=c(-1,2.5), main="SAPRTE") # doesn't cross 0
abline(h=0, lty=2)
plot(saprte.obs$Comp1, saprte.rmf.pc1,type="l", xlab="Soil Component 1",
     ylab="Slope of RMF-RGR Relationship") # crosses 0
plot(saprte.obs$Comp2, saprte.rmf.pc2,type="l", xlab="Soil Component 2",
     ylab="Slope of RMF-RGR Relationship") # crosses 0
plot(saprte.obs$Comp2, saprte.ssl.pc2,type="l", xlab="Soil Component 2",
     ylab="Slope of SSL-RGR Relationship") # crosses 0

# Slopes for three-way interaction models

saprte.lma.rmf.pc1=saprte[39,2]+saprte[51,2]*saprte.obs$log_rmf+saprte[53,2]*saprte.obs$Comp1+
  saprte[71,2]*(saprte.obs$log_rmf*saprte.obs$Comp1)

# Plotting slopes against other trait/environment

plot((saprte.obs$log_rmf*saprte.obs$Comp1),saprte.lma.rmf.pc1,xlab="RMF x Soil Component 1",
     ylab="Slope of LMA-RGR Relationship") # 4 points cross 0

# Regression to get slope and intercept value for plotting of mean

saprte.lma.light.lm=lm(saprte.lma.light~saprte.obs$log_light)
saprte.lma.light.lm$coefficients[1] # -1.064056 Intercept
saprte.lma.light.lm$coefficients[2] # 0.4035422 Slope

saprte.smf.light.lm=lm(saprte.smf.light~saprte.obs$log_light)
saprte.smf.light.lm$coefficients[1] # 1.134558 Intercept
saprte.smf.light.lm$coefficients[2] # 0.6164366 Slope

saprte.rmf.pc1.lm=lm(saprte.rmf.pc1~saprte.obs$Comp1)
saprte.rmf.pc1.lm$coefficients[1] # -0.6893811 Intercept
saprte.rmf.pc1.lm$coefficients[2] # 0.4105834 Slope

saprte.rmf.pc2.lm=lm(saprte.rmf.pc2~saprte.obs$Comp2)
saprte.rmf.pc2.lm$coefficients[1] # -0.6893811  Intercept
saprte.rmf.pc2.lm$coefficients[2] # 0.9896442 Slope

saprte.ssl.pc2.lm=lm(saprte.ssl.pc2~saprte.obs$Comp2)
saprte.ssl.pc2.lm$coefficients[1] # -1.328428 Intercept
saprte.ssl.pc2.lm$coefficients[2] # 0.9075441 Slope

saprte.lma.rmf.pc1.lm=lm(saprte.lma.rmf.pc1~saprte.obs$log_rmf*saprte.obs$Comp1)
saprte.lma.rmf.pc1.lm$coefficients[1] # -1.064056 Intercept
saprte.lma.rmf.pc1.lm$coefficients[2] # 0.9316986 Slope

# Code to produce 95% credible intervals around first partial derivative slopes

saprte.lma.light.slopes=matrix(data=NA, nrow=54, ncol=1000)
saprte.smf.light.slopes=matrix(data=NA, nrow=54, ncol=1000)
saprte.rmf.pc1.slopes=matrix(data=NA, nrow=54, ncol=1000)
saprte.rmf.pc2.slopes=matrix(data=NA, nrow=54, ncol=1000)
saprte.ssl.pc2.slopes=matrix(data=NA, nrow=54, ncol=1000)
saprte.lma.rmf.pc1.slopes=matrix(data=NA, nrow=54, ncol=1000)

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.lma.light.output=row.sample[1]+row.sample[14]*saprte.obs$log_light
  saprte.lma.light.slopes[,i]=saprte.lma.light.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.smf.light.output=row.sample[7]+row.sample[20]*saprte.obs$log_light
  saprte.smf.light.slopes[,i]=saprte.smf.light.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.rmf.pc1.output=row.sample[2]+row.sample[24]*saprte.obs$Comp1
  saprte.rmf.pc1.slopes[,i]=saprte.rmf.pc1.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.rmf.pc2.output=row.sample[2]+row.sample[25]*saprte.obs$Comp2
  saprte.rmf.pc2.slopes[,i]=saprte.rmf.pc2.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.ssl.pc2.output=row.sample[4]+row.sample[27]*saprte.obs$Comp2
  saprte.ssl.pc2.slopes[,i]=saprte.ssl.pc2.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.lma.rmf.pc1.output=row.sample[1]+row.sample[13]*saprte.obs$log_rmf+
    row.sample[15]*saprte.obs$Comp1+row.sample[33]*(saprte.obs$log_rmf*saprte.obs$Comp1)
  saprte.lma.rmf.pc1.slopes[,i]=saprte.lma.rmf.pc1.output
}

saprte.lma.light.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
saprte.smf.light.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
saprte.rmf.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
saprte.rmf.pc2.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
saprte.ssl.pc2.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
saprte.lma.rmf.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)

for(i in 1:1000){
  lin.mod=lm(saprte.lma.light.slopes[,i]~saprte.obs$log_light)
  saprte.lma.light.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.lma.light.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(saprte.smf.light.slopes[,i]~saprte.obs$log_light)
  saprte.smf.light.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.smf.light.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(saprte.rmf.pc1.slopes[,i]~saprte.obs$Comp1)
  saprte.rmf.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.rmf.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(saprte.rmf.pc2.slopes[,i]~saprte.obs$Comp2)
  saprte.rmf.pc2.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.rmf.pc2.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(saprte.ssl.pc2.slopes[,i]~saprte.obs$Comp2)
  saprte.ssl.pc2.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.ssl.pc2.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(saprte.lma.rmf.pc1.slopes[,i]~saprte.obs$log_rmf*saprte.obs$Comp1)
  saprte.lma.rmf.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  saprte.lma.rmf.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

saprte.lma.light.quant.intercept=quantile(saprte.lma.light.lm.1000[,1], probs=c(0.025, 0.975))
saprte.lma.light.quant.slope=quantile(saprte.lma.light.lm.1000[,2], probs=c(0.025, 0.975))

saprte.smf.light.quant.intercept=quantile(saprte.smf.light.lm.1000[,1], probs=c(0.025, 0.975))
saprte.smf.light.quant.slope=quantile(saprte.smf.light.lm.1000[,2], probs=c(0.025, 0.975))

saprte.rmf.pc1.quant.intercept=quantile(saprte.rmf.pc1.lm.1000[,1], probs=c(0.025, 0.975))
saprte.rmf.pc1.quant.slope=quantile(saprte.rmf.pc1.lm.1000[,2], probs=c(0.025, 0.975))

saprte.rmf.pc2.quant.intercept=quantile(saprte.rmf.pc2.lm.1000[,1], probs=c(0.025, 0.975))
saprte.rmf.pc2.quant.slope=quantile(saprte.rmf.pc2.lm.1000[,2], probs=c(0.025, 0.975))

saprte.ssl.pc2.quant.intercept=quantile(saprte.ssl.pc2.lm.1000[,1], probs=c(0.025, 0.975))
saprte.ssl.pc2.quant.slope=quantile(saprte.ssl.pc2.lm.1000[,2], probs=c(0.025, 0.975))

saprte.lma.rmf.pc1.quant.intercept=quantile(saprte.lma.rmf.pc1.lm.1000[,1], probs=c(0.025, 0.975))
saprte.lma.rmf.pc1.quant.slope=quantile(saprte.lma.rmf.pc1.lm.1000[,2], probs=c(0.025, 0.975))

plot(saprte.obs$log_light, saprte.lma.light, type="n", ylim=c(-8,5.6)) ## CI of slope includes 0
abline(-1.064056, 0.4035422, col="black") # mean value
abline(-7.957438,-0.01848265, col="gray") # lower limit
abline(5.518632,0.76346751, col="gray") # upper limit

plot(saprte.obs$log_light, saprte.smf.light, type="n", ylim=c(0,2.3))
abline(1.134558, 0.6164366, col="black") # mean value
abline(0.01226997,0.0983762, col="gray") # lower limit
abline(2.27040324,1.1364356, col="gray") # upper limit

plot(saprte.obs$Comp1, saprte.rmf.pc1, type="n", ylim=c(-2.3,0.9))
abline(-0.6893811, 0.4105834, col="black") # mean value
abline(-2.2830971,0.05446718, col="gray") # lower limit
abline(0.8991783,0.73160326, col="gray") # upper limit

plot(saprte.obs$Comp2, saprte.rmf.pc2, type="n", ylim=c(-2.2,0.9))
abline(-0.6893811, 0.9896442, col="black") # mean value
abline(-2.1527916,0.2514345, col="gray") # lower limit
abline(0.8206142 ,1.7727568, col="gray") # upper limit

plot(saprte.obs$Comp2, saprte.ssl.pc2, type="n", ylim=c(-2.5,-0.25))
abline(-1.328428, 0.9075441, col="black") # mean value
abline(-2.4142057,0.2827095, col="gray") # lower limit
abline(-0.2919049,1.5336651, col="gray") # upper limit

plot((saprte.obs$log_rmf*saprte.obs$Comp1),saprte.lma.rmf.pc1, type="n", ylim=c(-7.7,5.5)) ## CI of slope includes 0
abline(-1.064056, 0.9316986, col="black") # mean value
abline(-7.600622,-0.3767483, col="gray") # lower limit
abline(5.437836,2.0203091, col="gray") # upper limit

## Observed ranges of traits and environmental variables

range(saprte.obs$log_light) # -1.880860  2.101726
range(saprte.obs$Comp1) # -1.813971  5.169627
range(saprte.obs$Comp2) # -2.327452  2.727038
range(saprte.obs$log_rmf) # -2.254598  2.016096

#Slope
saprte.lma.light.High=saprte[39,2]+saprte[52,2]*2.101726
-0.2159212
saprte.lma.light.Low=saprte[39,2]+saprte[52,2]*-1.880860
-1.823063

saprte.smf.light.High=saprte[45,2]+saprte[58,2]*2.101726
2.430139
saprte.smf.light.Low=saprte[45,2]+saprte[58,2]*-1.880860
-0.024873

saprte.rmf.pc1.High=saprte[40,2]+saprte[62,2]*5.169627
1.433182
saprte.rmf.pc1.Low=saprte[40,2]+saprte[62,2]*-1.813971 
-1.434167

saprte.rmf.pc2.High=saprte[40,2]+saprte[63,2]*2.727038
2.009416
saprte.rmf.pc2.Low=saprte[40,2]+saprte[63,2]*-2.327452
-2.99273

saprte.ssl.pc2.High=saprte[42,2]+saprte[65,2]*2.727038
1.146479
saprte.ssl.pc2.Low=saprte[42,2]+saprte[65,2]*-2.327452
-3.440693

saprte.lma.rmf.pc1.HH=saprte[39,2]+saprte[51,2]*2.016096+saprte[53,2]*5.169627+saprte[71,2]*(2.016096*5.169627)
-3.371453
saprte.lma.rmf.pc1.LH=saprte[39,2]+saprte[51,2]*-2.254598+saprte[53,2]*5.169627+saprte[71,2]*(-2.254598*5.169627)
2.959369
saprte.lma.rmf.pc1.LL=saprte[39,2]+saprte[51,2]*-2.254598+saprte[53,2]*-1.813971 +saprte[71,2]*(-2.254598*-1.813971)
-5.313524
saprte.lma.rmf.pc1.HL=saprte[39,2]+saprte[51,2]*2.016096+saprte[53,2]*-1.813971 +saprte[71,2]*(2.016096*-1.813971)
2.28309

saprte.lma.rmf.pc1.slopes.HH=matrix(data=NA, nrow=1000, ncol=1)
saprte.lma.rmf.pc1.slopes.LH=matrix(data=NA, nrow=1000, ncol=1)
saprte.lma.rmf.pc1.slopes.LL=matrix(data=NA, nrow=1000, ncol=1)
saprte.lma.rmf.pc1.slopes.HL=matrix(data=NA, nrow=1000, ncol=1)

for(i in 1:1000){
  row.sample=as.numeric(post.saprte.2[sample(nrow(post.saprte.2), 1),])
  saprte.lma.rmf.pc1.output.HH=row.sample[1]+row.sample[13]*2.016096+row.sample[15]*5.169627+row.sample[33]*(2.016096*5.169627)
  saprte.lma.rmf.pc1.output.LH=row.sample[1]+row.sample[13]*-2.254598+row.sample[15]*5.169627+row.sample[33]*(-2.254598*5.169627)
  saprte.lma.rmf.pc1.output.LL=row.sample[1]+row.sample[13]*-2.254598+row.sample[15]*-1.813971+row.sample[33]*(-2.254598*-1.813971)
  saprte.lma.rmf.pc1.output.HL=row.sample[1]+row.sample[13]*2.016096+row.sample[15]*-1.813971+row.sample[33]*(2.016096*-1.813971)
  saprte.lma.rmf.pc1.slopes.HH[i,]=saprte.lma.rmf.pc1.output.HH
  saprte.lma.rmf.pc1.slopes.LH[i,]=saprte.lma.rmf.pc1.output.LH
  saprte.lma.rmf.pc1.slopes.LL[i,]=saprte.lma.rmf.pc1.output.LL
  saprte.lma.rmf.pc1.slopes.HL[i,]=saprte.lma.rmf.pc1.output.HL
}

high.soil=cbind(saprte.lma.rmf.pc1.slopes.HH, saprte.lma.rmf.pc1.slopes.LH)
write.csv(high.soil, file="saprte.lma.rmf.pc1.highsoil.csv")
# 621/1000 have different slopes

HH.quantile.slope=quantile(saprte.lma.rmf.pc1.slopes.HH[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-10.606410   3.694795

LH.quantile.slope=quantile(saprte.lma.rmf.pc1.slopes.LH[,1], probs=c(0.025, 0.975))
#  2.5%     97.5% 
-5.934807 10.000002 

low.soil=cbind(saprte.lma.rmf.pc1.slopes.LL, saprte.lma.rmf.pc1.slopes.HL)
write.csv(low.soil, file="saprte.lma.rmf.pc1.lowsoil.csv")
# 670/1000 have different slopes

LL.quantile.slope=quantile(saprte.lma.rmf.pc1.slopes.LL[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-13.348332   2.254375
HL.quantile.slope=quantile(saprte.lma.rmf.pc1.slopes.HL[,1], probs=c(0.025, 0.975))
# 2.5%     97.5% 
-6.281514  9.004141

# test for significant difference between mean slopes
t.test(saprte.lma.rmf.pc1.slopes.HH,saprte.lma.rmf.pc1.slopes.LH)
# p < 0.0001
t = -36.396
df = 1994
CI = -6.579017 -5.906265
HH mean = -3.484167
LH mean = 2.758473

t.test(saprte.lma.rmf.pc1.slopes.LL,saprte.lma.rmf.pc1.slopes.HL)
# p < 0.0001
t = -42.416
df = 1981.5
CI = -7.792435 -7.103697
LL mean = -5.392299
HL mean = 2.055767 

# Plotting
install.packages("visreg")
library(visreg)

saprte.lma.light.mod=lm(log_rgr~log_lma*log_light, data=saprte.obs)
saprte.smf.light.mod=lm(log_rgr~log_smf*log_light, data=saprte.obs)
saprte.rmf.pc1.mod=lm(log_rgr~log_rmf*Comp1, data=saprte.obs)
saprte.rmf.pc2.mod=lm(log_rgr~log_rmf*Comp2, data=saprte.obs)
saprte.ssl.pc2.mod=lm(log_rgr~log_ssl*Comp2, data=saprte.obs)

visreg2d(saprte.lma.light.mod, "log_lma", "log_light", plot.type="image", xlab="Leaf Mass per Area", ylab="Light", main="SAPRTE")
visreg2d(saprte.lma.light.mod, "log_lma", "log_light", plot.type="persp", ylab="Light", zlab="\nRelative Growth Rate", xlab="Leaf Mass per Area", main="SAPRTE",cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5))
visreg2d(saprte.smf.light.mod, "log_smf", "log_light", plot.type="image", xlab="Stem Mass Fraction", ylab="Light", main="SAPRTE")
visreg2d(saprte.smf.light.mod, "log_smf", "log_light", plot.type="persp", ylab="Light", zlab="\nRelative Growth Rate", xlab="Stem Mass Fraction", main="SAPRTE",cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5))
visreg2d(saprte.rmf.pc1.mod, "log_rmf", "Comp1", plot.type="image", xlab="Root Mass Fraction", ylab="Soil Comp 1", main="SAPRTE")
visreg2d(saprte.rmf.pc1.mod, "log_rmf", "Comp1", plot.type="persp", ylab="Soil Comp 1",
         zlab="\nRelative Growth Rate", xlab="Root Mass Fraction", main="SAPRTE",cex.main=1,nn=99,
         cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")
visreg2d(saprte.rmf.pc2.mod, "log_rmf", "Comp2", plot.type="image", xlab="Root Mass Fraction", ylab="Soil Comp 2", main="SAPRTE")
visreg2d(saprte.rmf.pc2.mod, "log_rmf", "Comp2", plot.type="persp", ylab="Soil Comp 2",
         zlab="\nRelative Growth Rate", xlab="Root Mass Fraction", main="SAPRTE",cex.main=1,nn=99,
         cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")
visreg2d(saprte.ssl.pc2.mod, "log_ssl", "Comp2", plot.type="image", xlab="Specific Stem Length", ylab="Soil Comp 2", main="SAPRTE")
visreg2d(saprte.ssl.pc2.mod, "log_ssl", "Comp2", plot.type="persp", ylab="Soil Comp 2",
         zlab="\nRelative Growth Rate", xlab="Specific Stem Length", main="SAPRTE",
         cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5),ticktype="simple")

saprte.lma.rmf.pc1.mod=lm(log_rgr~log_lma*log_rmf*Comp1, data=saprte.obs)
library(effects)
saprte.lma.rmf.pc1.effects=allEffects(saprte.lma.rmf.pc1.mod)
plot(saprte.lma.rmf.pc1.effects)

plot(predictorEffects(saprte.lma.rmf.pc1.mod, ~ log_lma, xlevels = list(log_rmf=c(-2,2))),
     index.cond=list(c(3,2,1,5,4)),lines=list(multiline=TRUE),
     lattice=list(key.args=list(title ="Root Mass Fraction")),
     axes=list(grid=FALSE, x=list(rug=FALSE)), xlab="Leaf Mass per Area",
     ylab="Relative Growth Rate", main="")






