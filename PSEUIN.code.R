## PSEUIN slopes

#### Read in observed values ####
setwd("~/Documents/GitHub/Intraspecific.Designs")
pseuin.obs=read.csv("PSEUIN.data.csv", header=T, row.names=1)

#### Slopes for two-way interaction model ####

pseuin.lma.pc1=pseuin[137,2]+pseuin[145,2]*pseuin.obs$Comp1
pseuin.rmf.light=pseuin[138,2]+pseuin[147,2]*pseuin.obs$log_light

# Plotting slopes against other trait/environment

plot(pseuin.obs$Comp1, pseuin.lma.pc1, type="l", xlab="Soil Component 1",
     ylab="Slope of LMA-RGR Relationship") # crosses 0
plot(pseuin.obs$log_light, pseuin.rmf.light, type="l",xlab="Light Availability",
     ylab="Slope of LMA-RGR Relationship") # barely crosses 0

# Regression to get slope and intercept value for plotting of mean

pseuin.lma.Comp1.lm=lm(pseuin.lma.pc1~pseuin.obs$Comp1)
pseuin.lma.Comp1.lm$coefficients[1] # -0.00066662  Intercept
pseuin.lma.Comp1.lm$coefficients[2] # -0.05638288  Slope

pseuin.rmf.light.lm=lm(pseuin.rmf.light~pseuin.obs$log_light)
pseuin.rmf.light.lm$coefficients[1] # -0.1977182  Intercept
pseuin.rmf.light.lm$coefficients[2] # -0.1292348 Slope

# Code to produce 95% credible intervals around first partial derivative slopes

pseuin.lma.pc1.slopes=matrix(data=NA, nrow=373, ncol=1000)
pseuin.rmf.light.slopes=matrix(data=NA, nrow=373, ncol=1000)

for(i in 1:1000){
  row.sample=as.numeric(post.pseuin.2[sample(nrow(post.pseuin.2), 1),])
  pseuin.lma.smf.output=row.sample[1]+row.sample[12]*pseuin.obs$log_smf
  pseuin.lma.pc1.output=row.sample[1]+row.sample[15]*pseuin.obs$Comp1
  pseuin.lma.smf.slopes[,i]=pseuin.lma.smf.output
  pseuin.lma.pc1.slopes[,i]=pseuin.lma.pc1.output
}

pseuin.lma.smf.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
pseuin.lma.pc1.lm.1000=matrix(data=NA, ncol=2, nrow=1000)

for(i in 1:1000){
  lin.mod=lm(pseuin.lma.smf.slopes[,i]~pseuin.obs$log_smf)
  pseuin.lma.smf.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  pseuin.lma.smf.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(pseuin.lma.pc1.slopes[,i]~pseuin.obs$Comp1)
  pseuin.lma.pc1.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  pseuin.lma.pc1.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

pseuin.lma.smf.quant.intercept=quantile(pseuin.lma.smf.lm.1000[,1], probs=c(0.025, 0.975))
pseuin.lma.smf.quant.slope=quantile(pseuin.lma.smf.lm.1000[,2], probs=c(0.025, 0.975))

pseuin.lma.pc1.quant.intercept=quantile(pseuin.lma.pc1.lm.1000[,1], probs=c(0.025, 0.975))
pseuin.lma.pc1.quant.slope=quantile(pseuin.lma.pc1.lm.1000[,2], probs=c(0.025, 0.975))

plot(pseuin.obs$log_smf,pseuin.lma.smf, type="n", ylim=c(-8,8))
abline(0.06685945, 0.1129412, col="black") # mean value
abline(-7.500424,0.01302107, col="gray") # lower limit
abline(7.280611,0.21006200, col="gray") # upper limit

plot(pseuin.obs$Comp1,pseuin.lma.pc1, type="n", ylim=c(-8,8))
abline(0.06685945, -0.06748557, col="black") # mean value
abline(-7.500424,-0.123322721, col="gray") # lower limit
abline(7.280611,-0.004715485, col="gray") # upper limit

#### Effect Plots ####
## Observed ranges of traits and environmental variables

range(pseuin.obs$log_rmf) # -3.462163  2.229758
range(pseuin.obs$log_lma) # -4.013001  4.416575
range(pseuin.obs$Comp1) # -6.062326  5.732475
range(pseuin.obs$log_light) # -2.344761  2.813687

#Slope
pseuin.lma.Comp1.High=pseuin[137,2]+pseuin[148,2]*3.820216
0.4983191
pseuin.lma.Comp1.Low=pseuin[137,2]+pseuin[148,2]*-3.462163
-0.3241612

pseuin.rmf.light.High=pseuin[137,2]+pseuin[151,2]*5.732475
-0.3199999
pseuin.rmf.light.Low=pseuin[137,2]+pseuin[151,2]*-6.062326
0.475979

#### Plotting ####
install.packages("visreg")
library(visreg)

pseuin.lma.pc1.mod=lm(log_rgr~log_lma*Comp1, data=pseuin.obs)
pseuin.rmf.light.mod=lm(log_rgr~log_rmf*log_light, data=pseuin.obs)
visreg2d(pseuin.lma.pc1.mod, "log_lma", "Comp1", plot.type="image", 
         xlab="Leaf Mass per Area", ylab="Soil Comp 1", main="PSEUIN")
visreg2d(pseuin.lma.pc1.mod, "log_lma", "Comp1", plot.type="persp", ylab="Soil Comp 1", 
         zlab="\nRelative Growth Rate", xlab="Leaf Mass per Area", main="PSEUIN",
         cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")
visreg2d(pseuin.rmf.light.mod, "log_rmf", "log_light", plot.type="image", 
         xlab="Root Mass Fraction", ylab="Light", main="PSEUIN")
visreg2d(pseuin.rmf.light.mod, "log_rmf", "log_light", plot.type="persp", 
         ylab="Light", zlab="\nRelative Growth Rate", xlab="Root Mass Fraction", main="PSEUIN",
         cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")

plot(allEffects(pseuin.rmf.light.mod))
