## WALSYU slopes

# Read in observed values

setwd("~/Desktop/Intrasp.perform.land/Outputs")
walsyu.obs=read.csv("walysu.csv", header=T, row.names=1)

# Slopes for two-way interaction model

walsyu.lmf.lar=walsyu[38,2]+walsyu[49,2]*walsyu.obs$log_lar

# Plotting slopes against other trait/environment

plot(walsyu.obs$log_lar, walsyu.lmf.lar,type="l", xlab="Leaf Area Ratio",
     ylab="Slope of LMF-RGR Relationship") # barely crosses 0

# Slopes for three-way interaction models

walsyu.lma.smf.light=walsyu[33,2]+walsyu[44,2]*walsyu.obs$log_smf+walsyu[46,2]*walsyu.obs$log_light+
  walsyu[63,2]*(walsyu.obs$log_smf*walsyu.obs$log_light)

# Plotting slopes against other trait/environment

plot((walsyu.obs$log_smf*walsyu.obs$log_light),walsyu.lma.smf.light, xlab="SMF x Light",
     ylab="Slope of LMA-RGR Relationship") # barely crosses 0

# Regression to get slope and intercept value for plotting of mean

walsyu.lmf.lar.lm=lm(walsyu.lmf.lar~walsyu.obs$log_lar)
walsyu.lmf.lar.lm$coefficients[1] # -1.269514  Intercept
walsyu.lmf.lar.lm$coefficients[2] # -0.6342963 Slope

walsyu.lma.smf.light.lm=lm(walsyu.lma.smf.light~walsyu.obs$log_smf*walsyu.obs$log_light)
walsyu.lma.smf.light.lm$coefficients[1] # -0.4736677 Intercept
walsyu.lma.smf.light.lm$coefficients[2] # -0.03750619 Slope

# Code to produce 95% credible intervals around first partial derivative slopes

walsyu.lmf.lar.slopes=matrix(data=NA, nrow=80, ncol=1000)
walsyu.lma.smf.light.slopes=matrix(data=NA, nrow=80, ncol=1000)

for(i in 1:1000){
  row.sample=as.numeric(post.walsyu.2[sample(nrow(post.walsyu.2), 1),])
  walsyu.lmf.lar.output=row.sample[6]+row.sample[17]*walsyu.obs$log_lar
  walsyu.lmf.lar.slopes[,i]=walsyu.lmf.lar.output
}

for(i in 1:1000){
  row.sample=as.numeric(post.walsyu.2[sample(nrow(post.walsyu.2), 1),])
  walsyu.lma.smf.light.output=row.sample[1]+row.sample[12]*walsyu.obs$log_smf+
    row.sample[14]*walsyu.obs$log_light+row.sample[31]*(walsyu.obs$log_smf*walsyu.obs$log_light)
  walsyu.lma.smf.light.slopes[,i]=walsyu.lma.smf.light.output
}

walsyu.lmf.lar.lm.1000=matrix(data=NA, ncol=2, nrow=1000)
walsyu.lma.smf.light.lm.1000=matrix(data=NA, ncol=2, nrow=1000)

for(i in 1:1000){
  lin.mod=lm(walsyu.lmf.lar.slopes[,i]~walsyu.obs$log_lar)
  walsyu.lmf.lar.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  walsyu.lmf.lar.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

for(i in 1:1000){
  lin.mod=lm(walsyu.lma.smf.light.slopes[,i]~walsyu.obs$log_smf*walsyu.obs$log_light)
  walsyu.lma.smf.light.lm.1000[i,1]=lin.mod$coefficients[1] # intercept
  walsyu.lma.smf.light.lm.1000[i,2]=lin.mod$coefficients[2] # slope
}

walsyu.lmf.lar.quant.intercept=quantile(walsyu.lmf.lar.lm.1000[,1], probs=c(0.025, 0.975))
walsyu.lmf.lar.quant.slope=quantile(walsyu.lmf.lar.lm.1000[,2], probs=c(0.025, 0.975))

walsyu.lma.smf.light.quant.intercept=quantile(walsyu.lma.smf.light.lm.1000[,1], probs=c(0.025, 0.975))
walsyu.lma.smf.light.quant.slope=quantile(walsyu.lma.smf.light.lm.1000[,2], probs=c(0.025, 0.975))

plot(walsyu.obs$log_lar, walsyu.lmf.lar, type="n", ylim=c(-14.5,12.4))
abline(-1.269514, -0.6342963, col="black") # mean value
abline(-14.38960,-1.19917495, col="gray") # lower limit
abline(12.38391,-0.07795847, col="gray") # upper limit

plot((walsyu.obs$log_smf*walsyu.obs$log_light),walsyu.lma.smf.light, type="n", ylim=c(-6.1,5.5)) ## CI of slope includes 0
abline(-0.4736677, -0.03750619, col="black") # mean value
abline(-6.084663,-0.5696364, col="gray") # lower limit
abline(5.401581,0.4493469, col="gray") # upper limit

## Observed ranges of traits and environmental variables

range(walsyu.obs$log_lar) # -3.814275  1.969589
range(walsyu.obs$log_smf) # -2.521530  3.693066
range(walsyu.obs$log_light) # -2.295697  1.531642

#Slope
walsyu.lmf.lar.High=walsyu[38,2]+walsyu[49,2]*1.969589
-2.518817
walsyu.lmf.lar.Low=walsyu[38,2]+walsyu[49,2]*-3.814275 
1.149866

walsyu.lma.smf.light.HH=walsyu[33,2]+walsyu[44,2]*3.693066+walsyu[46,2]*1.531642+walsyu[63,2]*(3.693066*1.531642)
2.279934
walsyu.lma.smf.light.LH=walsyu[33,2]+walsyu[44,2]*-2.521530+walsyu[46,2]*1.531642+walsyu[63,2]*(-2.521530*1.531642)
-3.184893
walsyu.lma.smf.light.LL=walsyu[33,2]+walsyu[44,2]*-2.521530+walsyu[46,2]*-2.295697+walsyu[63,2]*(-2.521530*-2.295697)
3.826368
walsyu.lma.smf.light.HL=walsyu[33,2]+walsyu[44,2]*3.693066+walsyu[46,2]*-2.295697+walsyu[63,2]*(3.693066*-2.295697)
-4.947018

walsyu.lma.smf.light.slopes.HH=matrix(data=NA, nrow=1000, ncol=1)
walsyu.lma.smf.light.slopes.LH=matrix(data=NA, nrow=1000, ncol=1)
walsyu.lma.smf.light.slopes.LL=matrix(data=NA, nrow=1000, ncol=1)
walsyu.lma.smf.light.slopes.HL=matrix(data=NA, nrow=1000, ncol=1)

for(i in 1:1000){
  row.sample=as.numeric(post.walsyu.2[sample(nrow(post.walsyu.2), 1),])
  walsyu.lma.smf.light.output.HH=row.sample[1]+row.sample[12]*3.693066+row.sample[14]*1.531642+row.sample[31]*(3.693066*1.531642)
  walsyu.lma.smf.light.output.LH=row.sample[1]+row.sample[12]*-2.521530+row.sample[14]*1.531642+row.sample[31]*(-2.521530*1.531642)
  walsyu.lma.smf.light.output.LL=row.sample[1]+row.sample[12]*-2.521530+row.sample[14]*-2.295697+row.sample[31]*(-2.521530*-2.295697)
  walsyu.lma.smf.light.output.HL=row.sample[1]+row.sample[12]*3.693066+row.sample[14]*-2.295697+row.sample[31]*(3.693066*-2.295697)
  walsyu.lma.smf.light.slopes.HH[i,]=walsyu.lma.smf.light.output.HH
  walsyu.lma.smf.light.slopes.LH[i,]=walsyu.lma.smf.light.output.LH
  walsyu.lma.smf.light.slopes.LL[i,]=walsyu.lma.smf.light.output.LL
  walsyu.lma.smf.light.slopes.HL[i,]=walsyu.lma.smf.light.output.HL
}

high.soil=cbind(walsyu.lma.smf.light.slopes.HH, walsyu.lma.smf.light.slopes.LH)
write.csv(high.soil, file="walsyu.lma.smf.light.highsoil.csv")
# 578/1000 have different slopes

HH.quantile.slope=quantile(walsyu.lma.smf.light.slopes.HH[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-4.067441 10.058903

LH.quantile.slope=quantile(walsyu.lma.smf.light.slopes.LH[,1], probs=c(0.025, 0.975))
#  2.5%     97.5% 
-9.826921  3.933773 

low.soil=cbind(walsyu.lma.smf.light.slopes.LL, walsyu.lma.smf.light.slopes.HL)
write.csv(low.soil, file="walsyu.lma.smf.light.lowsoil.csv")
# 751/1000 have different slopes

LL.quantile.slope=quantile(walsyu.lma.smf.light.slopes.LL[,1], probs=c(0.025, 0.975))
#      2.5%     97.5% 
-2.666721 10.968308 
HL.quantile.slope=quantile(walsyu.lma.smf.light.slopes.HL[,1], probs=c(0.025, 0.975))
# 2.5%     97.5% 
-12.484863   3.235882 

# test for significant difference between mean slopes
t.test(walsyu.lma.smf.light.slopes.HH,walsyu.lma.smf.light.slopes.LH)
# p < 0.0001
t = 36.9
df = 1996.7
CI = 5.404209 6.010899
HH mean = 2.598345
LH mean = -3.109209

t.test(walsyu.lma.smf.light.slopes.LL,walsyu.lma.smf.light.slopes.HL)
# p < 0.0001
t = 53.351
df = 1987.4
CI = 8.683377 9.346136
LL mean = 4.099937
HL mean = -4.914820

# Slopes for three-way interaction models

walsyu.rmf.thick.pc1=walsyu[80,2]+walsyu[100,2]*walsyu.obs$log_thick+
  walsyu[102,2]*walsyu.obs$Comp1+walsyu[113,2]*(walsyu.obs$log_thick*walsyu.obs$Comp1)

# Plotting
install.packages("visreg")
library(visreg)

walsyu.lmf.lar.mod=lm(log_rgr~log_lmf*log_lar, data=walsyu.obs)

visreg2d(walsyu.lmf.lar.mod, "log_lmf", "log_lar", plot.type="image", xlab="Leaf Mass Fraction", ylab="Leaf Area Ratio", main="WALSYU")
visreg2d(walsyu.lmf.lar.mod, "log_lmf", "log_lar", plot.type="persp", ylab="Leaf Area Ratio", zlab="\nRelative Growth Rate", xlab="Leaf Mass Fraction",
         main="WALSYU",cex.main=1,nn=99, cex.lab=1,lwd=0.5, border="grey40", col=adjustcolor("blue",alpha.f=.5), ticktype="simple")


colnames(walsyu.obs)[26]="Light"

walsyu.lma.smf.light.mod=lm(log_rgr~log_lma*log_smf*Light, data=walsyu.obs)

library(effects)
walsyu.lma.smf.light.effects=allEffects(walsyu.lma.smf.light.mod)
plot(walsyu.lma.smf.light.effects)

walsyu.lma.smf.light.effects=allEffects(walsyu.lma.smf.light.mod)

plot(predictorEffects(walsyu.lma.smf.light.mod, ~ log_lma, xlevels = list(log_smf=c(-3,4))),
     index.cond=list(c(3,2,1,5,4)),lines=list(multiline=TRUE),
     lattice=list(key.args=list(title ="Stem Mass Fraction")),
     axes=list(grid=FALSE, x=list(rug=FALSE)), xlab="Leaf Mass per Area",
     ylab="Relative Growth Rate", main="")


