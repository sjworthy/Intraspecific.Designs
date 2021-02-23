## PITTKE slopes

# Read in observed values
setwd("~/Desktop/Intrasp.perform.land/Outputs")
pittke.obs=read.csv("pittke.csv", header=T, row.names=1)

# Slopes for two-way interaction model

pittke.lma.rmf=pittke[79,2]+pittke[91,2]*pittke.obs$log_rmf

# Plotting slopes against other trait/environment

plot(pittke.obs$log_rmf, pittke.lma.rmf,type="l", xlab="Root Mass Fraction",
     ylab="Slope of LMA-RGR Relationship") # crosses 0

# Slopes for three-way interaction models

pittke.rmf.thick.pc1=pittke[80,2]+pittke[100,2]*pittke.obs$log_thick+
  pittke[102,2]*pittke.obs$Comp1+pittke[113,2]*(pittke.obs$log_thick*pittke.obs$Comp1)

# Plotting slopes against other trait/environment

plot((pittke.obs$log_thick*pittke.obs$Comp1),pittke.rmf.thick.pc1, xlab="Thickness x Soil Component 1",
     ylab="Slope of RMF-RGR Relationship") # crosses 0

# Regression to get slope and intercept value for plotting of mean

pittke.lma.rmf.lm=lm(pittke.lma.rmf~pittke.obs$log_rmf)
pittke.lma.rmf.lm$coefficients[1] # -0.2217593 Intercept
pittke.lma.rmf.lm$coefficients[2] # -0.5400904 Slope

pittke.rmf.thick.pc1.lm=lm(pittke.rmf.thick.pc1~pittke.obs$log_thick*pittke.obs$Comp1)
pittke.rmf.thick.pc1.lm$coefficients[1] # -0.1402875 Intercept
pittke.rmf.thick.pc1.lm$coefficients[2] # 0.05068543 Slope

# Code to produce 95% credible intervals around first partial derivative slopes

pittke.lma.rmf.slopes=matrix(data=NA, nrow=108, ncol=1000)
pittke.rmf.thick.pc1.slopes=matrix(data=NA, nrow=108, ncol=1000)


for(i in 1:1000){
  row.sample=as.numeric(post.pittke.2[sample(nrow(post.pittke.2), 1),])
  pittke.lma.rmf.output=row.sample[1]+row.sample[13]*pittke.obs$log_rmf
  pittke.lma.rmf.slopes[,i]=pittke.lma.rmf.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.pittke.2[sample(nrow(post.pittke.2), 1),])
  pittke.rmf.thick.pc1.output=row.sample[2]+row.sample[22]*pittke.obs$log_thick+
    row.sample[24]*pittke.obs$Comp1+row.sample[35]*(pittke.obs$log_thick*pittke.obs$Comp1)
  pittke.rmf.thick.pc1.slopes[,i]=pittke.rmf.thick.pc1.output
}

pittke.lma.rmf.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
pittke.rmf.thick.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)

for(i in 1:1000){
  lin.mod=lm(pittke.lma.rmf.slopes[,i]~pittke.obs$log_rmf)
  pittke.lma.rmf.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  pittke.lma.rmf.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(pittke.rmf.thick.pc1.slopes[,i]~pittke.obs$log_thick*pittke.obs$Comp1)
  pittke.rmf.thick.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  pittke.rmf.thick.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

pittke.lma.rmf.quant.intercept=quantile(pittke.lma.rmf.lm.1000[,1], probs=c(0.025, 0.975))
pittke.lma.rmf.quant.slope=quantile(pittke.lma.rmf.lm.1000[,2], probs=c(0.025, 0.975))

pittke.rmf.thick.pc1.quant.intercept=quantile(pittke.rmf.thick.pc1.lm.1000[,1], probs=c(0.025, 0.975))
pittke.rmf.thick.pc1.quant.slope=quantile(pittke.rmf.thick.pc1.lm.1000[,2], probs=c(0.025, 0.975))

plot(pittke.obs$log_rmf, pittke.lma.rmf, type="n", ylim=c(-5.6,5))
abline(-0.2217593, -0.5400904, col="black") # mean value
abline(-5.538045,-0.99920402, col="gray") # lower limit
abline(4.916539,-0.06673128, col="gray") # upper limit

plot((pittke.obs$log_thick*pittke.obs$Comp1),pittke.rmf.thick.pc1, type="n", ylim=c(-.9,0.7)) ## CI of slope includes 0
abline( -0.1402875, 0.05068543, col="black") # mean value
abline(-0.8985220,-0.3966105, col="gray") # lower limit
abline(0.6918519,0.4836477, col="gray") # upper limit

## Observed ranges of traits and environmental variables

range(pittke.obs$log_rmf) # -2.582196  2.260574
range(pittke.obs$log_thick) # -2.586810  2.574494
range(pittke.obs$Comp1) # -3.698699  5.732475

#Slope
pittke.lma.rmf.High=pittke[79,2]+pittke[91,2]*2.260574
-1.442674
pittke.lma.rmf.Low=pittke[79,2]+pittke[91,2]*-2.582196
1.17286

pittke.rmf.thick.pc1.HH=pittke[80,2]+pittke[100,2]*2.574494+pittke[101,2]*5.732475+pittke[113,2]*(2.574494*5.732475)
-4.570159
pittke.rmf.thick.pc1.LH=pittke[80,2]+pittke[100,2]*-2.586810+pittke[102,2]*5.732475+pittke[113,2]*(-2.586810*5.732475)
3.770683
pittke.rmf.thick.pc1.LL=pittke[80,2]+pittke[100,2]*-2.586810+pittke[102,2]*-3.698699+pittke[113,2]*(-2.586810*-3.698699)
-2.879429
pittke.rmf.thick.pc1.HL=pittke[80,2]+pittke[100,2]*2.574494+pittke[102,2]*-3.698699+pittke[113,2]*(2.574494*-3.698699)
2.503552

pittke.rmf.thick.pc1.slopes.HH=matrix(data=NA, nrow=1000, ncol=1)
pittke.rmf.thick.pc1.slopes.LH=matrix(data=NA, nrow=1000, ncol=1)
pittke.rmf.thick.pc1.slopes.LL=matrix(data=NA, nrow=1000, ncol=1)
pittke.rmf.thick.pc1.slopes.HL=matrix(data=NA, nrow=1000, ncol=1)

for(i in 1:1000){
  row.sample=as.numeric(post.pittke.2[sample(nrow(post.pittke.2), 1),])
  pittke.rmf.thick.pc1.output.HH=row.sample[2]+row.sample[22]*2.574494+row.sample[24]*5.732475+row.sample[35]*(2.574494*5.732475)
  pittke.rmf.thick.pc1.output.LH=row.sample[2]+row.sample[22]*-2.586810+row.sample[24]*5.732475+row.sample[35]*(-2.586810*5.732475)
  pittke.rmf.thick.pc1.output.LL=row.sample[2]+row.sample[22]*-2.586810+row.sample[24]*-3.698699+row.sample[35]*(-2.586810*-3.698699)
  pittke.rmf.thick.pc1.output.HL=row.sample[2]+row.sample[22]*2.574494+row.sample[24]*-3.698699+row.sample[35]*(2.574494*-3.698699)
  pittke.rmf.thick.pc1.slopes.HH[i,]=pittke.rmf.thick.pc1.output.HH
  pittke.rmf.thick.pc1.slopes.LH[i,]=pittke.rmf.thick.pc1.output.LH
  pittke.rmf.thick.pc1.slopes.LL[i,]=pittke.rmf.thick.pc1.output.LL
  pittke.rmf.thick.pc1.slopes.HL[i,]=pittke.rmf.thick.pc1.output.HL
}

high.soil=cbind(pittke.rmf.thick.pc1.slopes.HH, pittke.rmf.thick.pc1.slopes.LH)
write.csv(high.soil, file="pittke.rmf.thick.pc1.highsoil.csv")
# 988/1000 have different slopes  # Two-Peaks

HH.quantile.slope=quantile(pittke.rmf.thick.pc1.slopes.HH[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-7.5251925 -0.4012538

LH.quantile.slope=quantile(pittke.rmf.thick.pc1.slopes.LH[,1], probs=c(0.025, 0.975))
#  2.5%     97.5% 
0.8777295 6.4750295

low.soil=cbind(pittke.rmf.thick.pc1.slopes.LL, pittke.rmf.thick.pc1.slopes.HL)
write.csv(low.soil, file="pittke.rmf.thick.pc1.lowsoil.csv")
# 904/1000 have different slopes

LL.quantile.slope=quantile(pittke.rmf.thick.pc1.slopes.LL[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-5.6883641  0.4060243 
HL.quantile.slope=quantile(pittke.rmf.thick.pc1.slopes.HL[,1], probs=c(0.025, 0.975))
# 2.5%     97.5% 
-1.071716  6.256315 

# test for significant difference between mean slopes
t.test(pittke.rmf.thick.pc1.slopes.HH,pittke.rmf.thick.pc1.slopes.LH)
# p < 0.0001
t = -101.35
df = 1923.7
CI = -7.516673 -7.231282
HH mean = -3.751911
LH mean = 3.622067  

t.test(pittke.rmf.thick.pc1.slopes.LL,pittke.rmf.thick.pc1.slopes.HL)
# p < 0.0001
t = -67.579
df = 1930.9
CI = -5.286685 -4.988493
LL mean = -2.753505
HL mean = 2.384083

# Plotting
install.packages("visreg")
library(visreg)

pittke.lma.rmf.mod=lm(log_rgr~log_lma*log_rmf, data=pittke.obs)

visreg2d(pittke.lma.rmf.mod, "log_lma", "log_rmf", plot.type="image", xlab="Leaf Mass per Area", ylab="Root Mass Fraction", main="PITTKE")
visreg2d(pittke.lma.rmf.mod, "log_lma", "log_rmf", plot.type="persp", ylab="Root Mass Fraction",
         zlab="\nRelative Growth Rate", xlab="Leaf Mass per Area", main="PITTKE",cex.main=1,nn=99,
         cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")

pittke.rmf.thick.pc1.mod=lm(log_rgr~log_rmf*log_thick*Comp1, data=pittke.obs)

library(effects)
pittke.rmf.thick.pc1.effects=allEffects(pittke.rmf.thick.pc1.mod)
plot(pittke.rmf.thick.pc1.effects)

plot(predictorEffects(pittke.rmf.thick.pc1.mod, ~ log_rmf, xlevels = list(log_thick=c(-3,3))),
     index.cond=list(c(3,2,1,5,4)),lines=list(multiline=TRUE),
     lattice=list(key.args=list(title ="Leaf Thickness")),
     axes=list(grid=FALSE, x=list(rug=FALSE)), xlab="Root Mass Fraction",
     ylab="Relative Growth Rate", main="")

