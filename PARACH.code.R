## PARACH slopes

# Read in observed values

setwd("~/Documents/GitHub/Intraspecific.Designs")
parach.obs=read.csv("PARACH.data.csv", header=T, row.names=1)

#### Slopes for two-way interaction model ####

parach.lma.rmf=parach[106,2]+parach[112,2]*parach.obs$log_rmf
parach.lma.rmf.pc2=parach[106,2]+parach[112,2]*parach.obs$log_rmf+parach[115,2]*parach.obs$Comp2+
  parach[121,2]*(parach.obs$log_rmf*parach.obs$Comp2)

# Plotting slopes against other trait/environment

plot(parach.obs$log_rmf, parach.lma.rmf,type="l", xlab="Root Mass Fraction",
     ylab="Slope of LMA-RGR Relationship") # crosses 0
plot((parach.obs$log_rmf*parach.obs$Comp2),parach.lma.rmf.pc2, xlab="RMF x Soil PC 2",
     ylab="Slope of LMA-RGR Relationship") # crosses 0

# Regression to get slope and intercept value for plotting of mean

parach.lma.pc1.lm=lm(parach.lma.pc1~parach.obs$Comp1)
parach.lma.pc1.lm$coefficients[1] # -0.01202049 Intercept
parach.lma.pc1.lm$coefficients[2] # 0.1262444 Slope

parach.rmf.pc1.lm=lm(parach.rmf.pc1~parach.obs$Comp1)
parach.rmf.pc1.lm$coefficients[1] # 0.003273919  Intercept
parach.rmf.pc1.lm$coefficients[2] # 0.09553588 Slope

parach.ssl.pc2.lm=lm(parach.ssl.pc2~parach.obs$Comp2)
parach.ssl.pc2.lm$coefficients[1] # -0.05868741 Intercept
parach.ssl.pc2.lm$coefficients[2] # 0.1401944 Slope

parach.thick.pc1.lm=lm(parach.thick.pc1~parach.obs$Comp1)
parach.thick.pc1.lm$coefficients[1] # -0.1654873 Intercept
parach.thick.pc1.lm$coefficients[2] # -0.1183111Slope

parach.lma.rmf.pc2.lm=lm(parach.lma.rmf.pc2~parach.obs$log_rmf*parach.obs$Comp2)
parach.lma.rmf.pc2.lm$coefficients[1] # -0.01202049 Intercept
parach.lma.rmf.pc2.lm$coefficients[2] # -0.1425948 Slope

# Code to produce 95% credible intervals around first partial derivative slopes

parach.lma.pc1.slopes=matrix(data=NA, nrow=194, ncol=1000)
parach.rmf.pc1.slopes=matrix(data=NA, nrow=194, ncol=1000)
parach.ssl.pc2.slopes=matrix(data=NA, nrow=194, ncol=1000)
parach.thick.pc1.slopes=matrix(data=NA, nrow=194, ncol=1000)
parach.lma.rmf.pc2.slopes=matrix(data=NA, nrow=194, ncol=1000)


for(i in 1:1000){
  row.sample=as.numeric(post.parach.2[sample(nrow(post.parach.2), 1),])
  parach.lma.pc1.output=row.sample[1]+row.sample[15]*parach.obs$Comp1
  parach.rmf.pc1.output=row.sample[2]+row.sample[24]*parach.obs$Comp1
  parach.ssl.pc2.output=row.sample[4]+row.sample[27]*parach.obs$Comp2
  parach.thick.pc1.output=row.sample[3]+row.sample[28]*parach.obs$Comp1
  parach.lma.pc1.slopes[,i]=parach.lma.pc1.output
  parach.rmf.pc1.slopes[,i]=parach.rmf.pc1.output
  parach.ssl.pc2.slopes[,i]=parach.ssl.pc2.output
  parach.thick.pc1.slopes[,i]=parach.thick.pc1.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.parach.2[sample(nrow(post.parach.2), 1),])
  parach.lma.rmf.pc2.output=row.sample[1]+row.sample[13]*parach.obs$log_rmf+
    row.sample[16]*parach.obs$Comp2+row.sample[36]*(parach.obs$log_rmf*parach.obs$Comp2)
  parach.lma.rmf.pc2.slopes[,i]=parach.lma.rmf.pc2.output
}

parach.lma.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
parach.rmf.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
parach.ssl.pc2.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
parach.thick.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
parach.lma.rmf.pc2.lm.1000=matrix(data=NA, ncol=2, nrow=1000)

for(i in 1:1000){
  lin.mod=lm(parach.lma.pc1.slopes[,i]~parach.obs$Comp1)
  parach.lma.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  parach.lma.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(parach.rmf.pc1.slopes[,i]~parach.obs$Comp1)
  parach.rmf.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  parach.rmf.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(parach.ssl.pc2.slopes[,i]~parach.obs$Comp2)
  parach.ssl.pc2.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  parach.ssl.pc2.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(parach.thick.pc1.slopes[,i]~parach.obs$Comp1)
  parach.thick.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  parach.thick.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(parach.lma.rmf.pc2.slopes[,i]~parach.obs$log_rmf*parach.obs$Comp2)
  parach.lma.rmf.pc2.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  parach.lma.rmf.pc2.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

parach.lma.pc1.quant.intercept=quantile(parach.lma.pc1.lm.1000[,1], probs=c(0.025, 0.975))
parach.lma.pc1.quant.slope=quantile(parach.lma.pc1.lm.1000[,2], probs=c(0.025, 0.975))

parach.rmf.pc1.quant.intercept=quantile(parach.rmf.pc1.lm.1000[,1], probs=c(0.025, 0.975))
parach.rmf.pc1.quant.slope=quantile(parach.rmf.pc1.lm.1000[,2], probs=c(0.025, 0.975))

parach.ssl.pc2.quant.intercept=quantile(parach.ssl.pc2.lm.1000[,1], probs=c(0.025, 0.975))
parach.ssl.pc2.quant.slope=quantile(parach.ssl.pc2.lm.1000[,2], probs=c(0.025, 0.975))

parach.thick.pc1.quant.intercept=quantile(parach.thick.pc1.lm.1000[,1], probs=c(0.025, 0.975))
parach.thick.pc1.quant.slope=quantile(parach.thick.pc1.lm.1000[,2], probs=c(0.025, 0.975))

parach.lma.rmf.pc2.quant.intercept=quantile(parach.lma.rmf.pc2.lm.1000[,1], probs=c(0.025, 0.975))
parach.lma.rmf.pc2.quant.slope=quantile(parach.lma.rmf.pc2.lm.1000[,2], probs=c(0.025, 0.975))

plot(parach.obs$Comp1, parach.lma.pc1, type="n", ylim=c(-5.5,5.5))
abline(-0.01202049, 0.1262444, col="black") # mean value
abline(-5.130764,0.02710347, col="gray") # lower limit
abline(5.030099,0.23667645, col="gray") # upper limit

plot(parach.obs$Comp1,parach.rmf.pc1, type="n", ylim=c(-0.5,0.4))
abline(0.003273919, 0.09553588, col="black") # mean value
abline(-0.4068480,0.006728408, col="gray") # lower limit
abline(0.3695262,0.192658202, col="gray") # upper limit

plot(parach.obs$Comp2,parach.ssl.pc2, type="n", ylim=c(-0.3,0.2))
abline(-0.05868741, 0.1401944, col="black") # mean value
abline(-0.2972469,0.05367291, col="gray") # lower limit
abline(0.1683021,0.24456514, col="gray") # upper limit

plot(parach.obs$Comp1,parach.thick.pc1, type="n", ylim=c(-0.4,0.05))
abline(-0.1654873, -0.1183111, col="black") # mean value
abline(-0.36661765,-0.1941830, col="gray") # lower limit
abline(0.04423583,-0.0493905, col="gray") # upper limit

plot((parach.obs$log_rmf*parach.obs$Comp2),parach.lma.rmf.pc2, type="n", ylim=c(-5,5.5)) ## CI of slope includes 0
abline(-0.01202049, -0.1425948, col="black") # mean value
abline(-4.939488,-0.31751327, col="gray") # lower limit
abline(5.123500,0.03598574, col="gray") # upper limit

## Observed ranges of traits and environmental variables

range(parach.obs$Comp1) # -6.062326  5.002055
range(parach.obs$Comp2) # -3.993391  3.370313
range(parach.obs$log_rmf) # -2.662866  2.665313

#Slope
parach.lma.pc1.High=parach[106,2]+parach[120,2]*5.002055
0.6194608
parach.lma.pc1.Low=parach[106,2]+parach[120,2]*-6.062326
-0.7773551

parach.rmf.pc1.High=parach[107,2]+parach[129,2]*5.002055
0.4811497
parach.rmf.pc1.Low=parach[107,2]+parach[129,2]*-6.062326
-0.5758957

parach.ssl.pc2.High=parach[109,2]+parach[132,2]*3.370313
0.4138116
parach.ssl.pc2.Low=parach[109,2]+parach[132,2]*-3.993391
-0.6185385

parach.thick.pc1.High=parach[108,2]+parach[133,2]*5.002055
-0.7572858
parach.thick.pc1.Low=parach[108,2]+parach[133,2]*-6.062326 
0.5517531

parach.lma.rmf.pc2.HH=parach[106,2]+parach[118,2]*2.665313+parach[121,2]*3.370313+parach[141,2]*(2.665313*3.370313)
0.650862
parach.lma.rmf.pc2.LH=parach[106,2]+parach[118,2]*-2.662866+parach[121,2]*3.370313+parach[141,2]*(-2.662866*3.370313)
-1.400372
parach.lma.rmf.pc2.LL=parach[106,2]+parach[118,2]*-2.662866+parach[121,2]*-3.993391+parach[141,2]*(-2.662866*-3.993391)
2.462619
parach.lma.rmf.pc2.HL=parach[106,2]+parach[118,2]*2.665313+parach[121,2]*-3.993391+parach[141,2]*(2.665313*-3.993391)
-1.627834

parach.lma.rmf.pc2.slopes.HH=matrix(data=NA, nrow=1000, ncol=1)
parach.lma.rmf.pc2.slopes.LH=matrix(data=NA, nrow=1000, ncol=1)
parach.lma.rmf.pc2.slopes.LL=matrix(data=NA, nrow=1000, ncol=1)
parach.lma.rmf.pc2.slopes.HL=matrix(data=NA, nrow=1000, ncol=1)

for(i in 1:1000){
  row.sample=as.numeric(post.parach.2[sample(nrow(post.parach.2), 1),])
  parach.lma.rmf.pc2.output.HH=row.sample[1]+row.sample[13]*2.665313+row.sample[16]*3.370313+row.sample[36]*(2.665313*3.370313)
  parach.lma.rmf.pc2.output.LH=row.sample[1]+row.sample[13]*-2.662866+row.sample[16]*3.370313+row.sample[36]*(-2.662866*3.370313)
  parach.lma.rmf.pc2.output.LL=row.sample[1]+row.sample[13]*-2.662866+row.sample[16]*-3.993391+row.sample[36]*(-2.662866*-3.993391)
  parach.lma.rmf.pc2.output.HL=row.sample[1]+row.sample[13]*2.665313+row.sample[16]*-3.993391+row.sample[36]*(2.665313*-3.993391)
  parach.lma.rmf.pc2.slopes.HH[i,]=parach.lma.rmf.pc2.output.HH
  parach.lma.rmf.pc2.slopes.LH[i,]=parach.lma.rmf.pc2.output.LH
  parach.lma.rmf.pc2.slopes.LL[i,]=parach.lma.rmf.pc2.output.LL
  parach.lma.rmf.pc2.slopes.HL[i,]=parach.lma.rmf.pc2.output.HL
}

high.soil=cbind(parach.lma.rmf.pc2.slopes.HH, parach.lma.rmf.pc2.slopes.LH)
write.csv(high.soil, file="parach.lma.rmf.pc2.highsoil.2.csv")
# 85/1000 have different slopes

HH.quantile.slope=quantile(parach.lma.rmf.pc2.slopes.HH[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-18.89515  18.95832

LH.quantile.slope=quantile(parach.lma.rmf.pc2.slopes.LH[,1], probs=c(0.025, 0.975))
#  2.5%     97.5% 
-21.13161  17.06982 

low.soil=cbind(parach.lma.rmf.pc2.slopes.LL, parach.lma.rmf.pc2.slopes.HL)
write.csv(low.soil, file="parach.lma.rmf.pc2.lowsoil.2.csv")
# 150/1000 have different slopes

LL.quantile.slope=quantile(parach.lma.rmf.pc2.slopes.LL[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-19.02891  23.55345
HL.quantile.slope=quantile(parach.lma.rmf.pc2.slopes.HL[,1], probs=c(0.025, 0.975))
# 2.5%     97.5% 
-23.14270  19.05603

# test for significant difference between mean slopes
t.test(parach.lma.rmf.pc2.slopes.HH,parach.lma.rmf.pc2.slopes.LH)
# p < 0.0001
t = 5.0564
df = 1998
CI = 1.301866 2.951595
HH mean = 0.5423858
LH mean = -1.5843446

t.test(parach.lma.rmf.pc2.slopes.LL,parach.lma.rmf.pc2.slopes.HL)
# p < 0.0001
t = 8.6484
df = 1998
CI = 3.220198 5.108956
LL mean = 2.463884
HL mean = -1.700693 

# Plotting
install.packages("visreg")
library(visreg)

parach.lma.rmf.mod=lm(log_rgr~log_lma*log_rmf, data=parach.obs)

visreg2d(parach.lma.rmf.mod, "log_lma", "log_rmf", plot.type="image", xlab="Leaf Mass per Area", 
         ylab="Root Mass Fraction", main="PARACH")
visreg2d(parach.lma.rmf.mod, "log_lma", "log_rmf", plot.type="persp", ylab="Root Mass Fraction", 
         zlab="\nRelative Growth Rate", xlab="Leaf Mass per Area",
         main="PARACH",cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")

parach.lma.rmf.pc2.mod=lm(log_rgr~log_lma*log_rmf*Comp2, data=parach.obs)

library(effects)

parach.lma.rmf.pc2.effects=allEffects(parach.lma.rmf.pc2.mod)

plot(predictorEffects(parach.lma.rmf.pc2.mod, ~ log_lma, xlevels = list(log_rmf=c(-3,3))),index.cond=list(c(3,2,1,5,4)),lines=list(multiline=TRUE),
     lattice=list(key.args=list(title ="Root Mass Fraction")),
     axes=list(grid=FALSE, x=list(rug=FALSE)), xlab="Leaf Mass per Area",
     ylab="Relative Growth Rate", main="")

plot(allEffects(parach.lma.rmf.pc2.mod))
