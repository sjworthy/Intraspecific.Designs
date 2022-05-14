library(picante)
library(rethinking)
library(rstan)

#### Read in the coefficient tables for each model ####
setwd("~/Documents/Intrasp.perform.land/intra.models/New.Results")

parach.coef=read.csv("parach.output.csv",header=T)
pittke.coef=read.csv("pittke.output.csv",header=T)
pseuin.coef=read.csv("pseuin.output.csv",header=T)

#### Read in the trait data for each model ####

setwd("~/Desktop/Intrasp.perform.land/Outputs")

parach.traits=read.csv("PARACH.data.csv",header=T)
pittke.traits=read.csv("PITTKE.data.csv",header=T)
pseuin.traits=read.csv("PSEUIN.data.csv",header=T)

# Growth performance of species based on contribution of each parameter
# Arnold 1983
# General equation form is beta of variable from coeff.table x mean value 
# of that trait(s) for that species + same thing for all variables

sp.perform.trait.df=matrix(data=NA, nrow=3,ncol=4)
colnames(sp.perform.trait.df)=c("traits.only", "all.parameters", "lma", "rmf")
row.names(sp.perform.trait.df)=c("parach", "pittke", "pseuin")

sp.perform.trait.df.2=as.data.frame(sp.perform.trait.df)
colnames(sp.perform.trait.df.2)[c(5:6)]=c("lma.min","rmf.min")
colnames(sp.perform.trait.df.2)[17:23]=c("lma.max","rmf.max")
colnames(sp.perform.trait.df.2)[24:26]=c("light.mean","pc1.mean","pc2.mean")
colnames(sp.perform.trait.df.2)[27:29]=c("light.min","pc1.min","pc2.min")
colnames(sp.perform.trait.df.2)[30:32]=c("light.max","pc1.max","pc2.max")
colnames(sp.perform.trait.df.2)[33:51]=c("lma.rmf.mean","lma.light.mean","lma.pc1.mean",
                                         "lma.pc2.mean","rmf.light.mean","rmf.pc1.mean","rmf.pc2.mean")

# Growth performance of species for each trait separately to see which one
# has the largest contribution to growth for each species
# mean, min, and max trait value used

# just traits
sp.perform.trait.df.2[1,3]=parach.coef[106,2]*mean(parach.traits$lma)
sp.perform.trait.df.2[1,4]=parach.coef[107,2]*mean(parach.traits$rmf)

sp.perform.trait.df.2[2,3]=pittke.coef[79,2]*mean(pittke.traits$lma)
sp.perform.trait.df.2[2,4]=pittke.coef[80,2]*mean(pittke.traits$rmf)

sp.perform.trait.df.2[3,3]=pseuin.coef[137,2]*mean(pseuin.traits$lma)
sp.perform.trait.df.2[3,4]=pseuin.coef[138,2]*mean(pseuin.traits$rmf)

sp.perform.trait.df.2[1,5]=parach.coef[106,2]*max(parach.traits$lma)
sp.perform.trait.df.2[1,6]=parach.coef[107,2]*max(parach.traits$rmf)

sp.perform.trait.df.2[2,5]=pittke.coef[79,2]*max(pittke.traits$lma)
sp.perform.trait.df.2[2,6]=pittke.coef[80,2]*max(pittke.traits$rmf)

sp.perform.trait.df.2[3,5]=pseuin.coef[137,2]*max(pseuin.traits$lma)
sp.perform.trait.df.2[3,6]=pseuin.coef[138,2]*max(pseuin.traits$rmf)

sp.perform.trait.df.2[1,7]=parach.coef[106,2]*min(parach.traits$lma)
sp.perform.trait.df.2[1,8]=parach.coef[107,2]*min(parach.traits$rmf)

sp.perform.trait.df.2[2,7]=pittke.coef[79,2]*min(pittke.traits$lma)
sp.perform.trait.df.2[2,8]=pittke.coef[80,2]*min(pittke.traits$rmf)

sp.perform.trait.df.2[3,7]=pseuin.coef[137,2]*min(pseuin.traits$lma)
sp.perform.trait.df.2[3,8]=pseuin.coef[138,2]*min(pseuin.traits$rmf)

colnames(sp.perform.trait.df.2)[5:6]=c("lma.max","rmf.max")
colnames(sp.perform.trait.df.2)[7:8]=c("lma.min","rmf.min")

# environmental data
sp.perform.trait.df.2[1,9]=parach.coef[109,2]*mean(parach.traits$perc.canopy.open)
sp.perform.trait.df.2[1,10]=parach.coef[110,2]*exp(mean(parach.traits$Comp1))
sp.perform.trait.df.2[1,11]=parach.coef[111,2]*exp(mean(parach.traits$Comp2))

sp.perform.trait.df.2[2,9]=pittke.coef[82,2]*mean(pittke.traits$perc.canopy.open)
sp.perform.trait.df.2[2,10]=pittke.coef[83,2]*exp(mean(pittke.traits$Comp1))
sp.perform.trait.df.2[2,11]=pittke.coef[84,2]*exp(mean(pittke.traits$Comp2))

sp.perform.trait.df.2[3,9]=pseuin.coef[140,2]*mean(pseuin.traits$perc.canopy.open)
sp.perform.trait.df.2[3,10]=pseuin.coef[141,2]*exp(mean(pseuin.traits$Comp1))
sp.perform.trait.df.2[3,11]=pseuin.coef[142,2]*exp(mean(pseuin.traits$Comp2))
                                                   
colnames(sp.perform.trait.df.2)[9:11]=c("light","Comp1", "Comp2")


sp.perform.trait.df.2[1,12]=parach.coef[109,2]*max(parach.traits$perc.canopy.open)
sp.perform.trait.df.2[1,13]=parach.coef[110,2]*exp(max(parach.traits$Comp1))
sp.perform.trait.df.2[1,14]=parach.coef[111,2]*exp(max(parach.traits$Comp2))

sp.perform.trait.df.2[2,12]=pittke.coef[82,2]*max(pittke.traits$perc.canopy.open)
sp.perform.trait.df.2[2,13]=pittke.coef[83,2]*exp(max(pittke.traits$Comp1))
sp.perform.trait.df.2[2,14]=pittke.coef[84,2]*exp(max(pittke.traits$Comp2))

sp.perform.trait.df.2[3,12]=pseuin.coef[140,2]*max(pseuin.traits$perc.canopy.open)
sp.perform.trait.df.2[3,13]=pseuin.coef[141,2]*exp(max(pseuin.traits$Comp1))
sp.perform.trait.df.2[3,14]=pseuin.coef[142,2]*exp(max(pseuin.traits$Comp2))

colnames(sp.perform.trait.df.2)[12:14]=c("max.light","max.Comp1", "max.Comp2")

sp.perform.trait.df.2[1,15]=parach.coef[109,2]*min(parach.traits$perc.canopy.open)
sp.perform.trait.df.2[1,16]=parach.coef[110,2]*exp(min(parach.traits$Comp1))
sp.perform.trait.df.2[1,17]=parach.coef[111,2]*exp(min(parach.traits$Comp2))

sp.perform.trait.df.2[2,15]=pittke.coef[82,2]*min(pittke.traits$perc.canopy.open)
sp.perform.trait.df.2[2,16]=pittke.coef[83,2]*exp(min(pittke.traits$Comp1))
sp.perform.trait.df.2[2,17]=pittke.coef[84,2]*exp(min(pittke.traits$Comp2))

sp.perform.trait.df.2[3,15]=pseuin.coef[140,2]*min(pseuin.traits$perc.canopy.open)
sp.perform.trait.df.2[3,16]=pseuin.coef[141,2]*exp(min(pseuin.traits$Comp1))
sp.perform.trait.df.2[3,17]=pseuin.coef[142,2]*exp(min(pseuin.traits$Comp2))

colnames(sp.perform.trait.df.2)[15:17]=c("min.light","min.Comp1", "min.Comp2")

# Growth performance of species for all functional traits combined

sp.perform.trait.df.2[1,1]=parach.coef[106,2]*mean(parach.traits$lma)+parach.coef[107,2]*mean(parach.traits$rmf)
sp.perform.trait.df.2[2,1]=pittke.coef[79,2]*mean(pittke.traits$lma)+pittke.coef[80,2]*mean(pittke.traits$rmf)
sp.perform.trait.df.2[3,1]=pseuin.coef[137,2]*mean(pseuin.traits$lma)+pseuin.coef[138,2]*mean(pseuin.traits$rmf)

sp.perform.trait.df.2[1,18]=parach.coef[106,2]*max(parach.traits$lma)+parach.coef[107,2]*max(parach.traits$rmf)
sp.perform.trait.df.2[2,18]=pittke.coef[79,2]*max(pittke.traits$lma)+pittke.coef[80,2]*max(pittke.traits$rmf)
sp.perform.trait.df.2[3,18]=pseuin.coef[137,2]*max(pseuin.traits$lma)+pseuin.coef[138,2]*max(pseuin.traits$rmf)

sp.perform.trait.df.2[1,19]=parach.coef[106,2]*min(parach.traits$lma)+parach.coef[107,2]*min(parach.traits$rmf)
sp.perform.trait.df.2[2,19]=pittke.coef[79,2]*min(pittke.traits$lma)+pittke.coef[80,2]*min(pittke.traits$rmf)
sp.perform.trait.df.2[3,19]=pseuin.coef[137,2]*min(pseuin.traits$lma)+pseuin.coef[138,2]*min(pseuin.traits$rmf)

colnames(sp.perform.trait.df.2)[18:19]=c("max.all.traits","min.all.traits")

# Example of calculation for 2-way interaction
sp.perform.trait.df.2[1,20]=parach.coef[112,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf))
sp.perform.trait.df.2[1,21]=parach.coef[112,2]*(min(parach.traits$lma)*min(parach.traits$rmf))
sp.perform.trait.df.2[1,22]=parach.coef[112,2]*(max(parach.traits$lma)*max(parach.traits$rmf))
sp.perform.trait.df.2[1,23]=parach.coef[112,2]*(min(parach.traits$lma)*max(parach.traits$rmf))
sp.perform.trait.df.2[1,24]=parach.coef[112,2]*(max(parach.traits$lma)*min(parach.traits$rmf))

sp.perform.trait.df.2[2,20]=pittke.coef[85,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf))
sp.perform.trait.df.2[2,21]=pittke.coef[85,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf))
sp.perform.trait.df.2[2,22]=pittke.coef[85,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf))
sp.perform.trait.df.2[2,23]=pittke.coef[85,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf))
sp.perform.trait.df.2[2,24]=pittke.coef[85,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf))

sp.perform.trait.df.2[3,20]=pseuin.coef[143,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf))
sp.perform.trait.df.2[3,21]=pseuin.coef[143,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf))
sp.perform.trait.df.2[3,22]=pseuin.coef[143,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf))
sp.perform.trait.df.2[3,23]=pseuin.coef[143,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf))
sp.perform.trait.df.2[3,24]=pseuin.coef[143,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf))

colnames(sp.perform.trait.df.2)[20:24]=c("lma.rmf.mean","lma.rmf.min","lma.rmf.max","lma.rmf.min.max","lma.rmf.max.min")

sp.perform.trait.df.2[1,25]=parach.coef[113,2]*(mean(parach.traits$lma)*mean(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,26]=parach.coef[113,2]*(min(parach.traits$lma)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,27]=parach.coef[113,2]*(max(parach.traits$lma)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,28]=parach.coef[113,2]*(min(parach.traits$lma)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,29]=parach.coef[113,2]*(max(parach.traits$lma)*min(parach.traits$perc.canopy.open))

sp.perform.trait.df.2[2,25]=pittke.coef[86,2]*(mean(pittke.traits$lma)*mean(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,26]=pittke.coef[86,2]*(min(pittke.traits$lma)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,27]=pittke.coef[86,2]*(max(pittke.traits$lma)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,28]=pittke.coef[86,2]*(min(pittke.traits$lma)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,29]=pittke.coef[86,2]*(max(pittke.traits$lma)*min(pittke.traits$perc.canopy.open))

sp.perform.trait.df.2[3,25]=pseuin.coef[144,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,26]=pseuin.coef[144,2]*(min(pseuin.traits$lma)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,27]=pseuin.coef[144,2]*(max(pseuin.traits$lma)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,28]=pseuin.coef[144,2]*(min(pseuin.traits$lma)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,29]=pseuin.coef[144,2]*(max(pseuin.traits$lma)*min(pseuin.traits$perc.canopy.open))

colnames(sp.perform.trait.df.2)[25:29]=c("lma.light.mean","lma.light.min","lma.light.max","lma.light.min.max","lma.light.max.min")

sp.perform.trait.df.2[1,30]=parach.coef[114,2]*(mean(parach.traits$lma)*exp(mean(parach.traits$Comp1)))
sp.perform.trait.df.2[1,31]=parach.coef[114,2]*(min(parach.traits$lma)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,32]=parach.coef[114,2]*(max(parach.traits$lma)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,33]=parach.coef[114,2]*(min(parach.traits$lma)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,34]=parach.coef[114,2]*(max(parach.traits$lma)*exp(min(parach.traits$Comp1)))

sp.perform.trait.df.2[2,30]=pittke.coef[87,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,31]=pittke.coef[87,2]*(min(pittke.traits$lma)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,32]=pittke.coef[87,2]*(max(pittke.traits$lma)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,33]=pittke.coef[87,2]*(min(pittke.traits$lma)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,34]=pittke.coef[87,2]*(max(pittke.traits$lma)*exp(min(pittke.traits$Comp1)))

sp.perform.trait.df.2[3,30]=pseuin.coef[145,2]*(mean(pseuin.traits$lma)*exp(mean(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,31]=pseuin.coef[145,2]*(min(pseuin.traits$lma)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,32]=pseuin.coef[145,2]*(max(pseuin.traits$lma)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,33]=pseuin.coef[145,2]*(min(pseuin.traits$lma)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,34]=pseuin.coef[145,2]*(max(pseuin.traits$lma)*exp(min(pseuin.traits$Comp1)))

colnames(sp.perform.trait.df.2)[30:34]=c("lma.Comp1.mean","lma.Comp1.min","lma.Comp1.max","lma.Comp1.min.max","lma.Comp1.max.min")

sp.perform.trait.df.2[1,35]=parach.coef[115,2]*(mean(parach.traits$lma)*exp(mean(parach.traits$Comp2)))
sp.perform.trait.df.2[1,36]=parach.coef[115,2]*(min(parach.traits$lma)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,37]=parach.coef[115,2]*(max(parach.traits$lma)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,38]=parach.coef[115,2]*(min(parach.traits$lma)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,39]=parach.coef[115,2]*(max(parach.traits$lma)*exp(min(parach.traits$Comp2)))

sp.perform.trait.df.2[2,35]=pittke.coef[88,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,36]=pittke.coef[88,2]*(min(pittke.traits$lma)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,37]=pittke.coef[88,2]*(max(pittke.traits$lma)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,38]=pittke.coef[88,2]*(min(pittke.traits$lma)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,39]=pittke.coef[88,2]*(max(pittke.traits$lma)*exp(min(pittke.traits$Comp2)))

sp.perform.trait.df.2[3,35]=pseuin.coef[146,2]*(mean(pseuin.traits$lma)*exp(mean(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,36]=pseuin.coef[146,2]*(min(pseuin.traits$lma)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,37]=pseuin.coef[146,2]*(max(pseuin.traits$lma)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,38]=pseuin.coef[146,2]*(min(pseuin.traits$lma)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,39]=pseuin.coef[146,2]*(max(pseuin.traits$lma)*exp(min(pseuin.traits$Comp2)))

colnames(sp.perform.trait.df.2)[35:39]=c("lma.Comp2.mean","lma.Comp2.min","lma.Comp2.max","lma.Comp2.min.max","lma.Comp2.max.min")

sp.perform.trait.df.2[1,40]=parach.coef[116,2]*(mean(parach.traits$rmf)*mean(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,41]=parach.coef[116,2]*(min(parach.traits$rmf)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,42]=parach.coef[116,2]*(max(parach.traits$rmf)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,43]=parach.coef[116,2]*(min(parach.traits$rmf)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,44]=parach.coef[116,2]*(max(parach.traits$rmf)*min(parach.traits$perc.canopy.open))

sp.perform.trait.df.2[2,40]=pittke.coef[89,2]*(mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,41]=pittke.coef[89,2]*(min(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,42]=pittke.coef[89,2]*(max(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,43]=pittke.coef[89,2]*(min(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,44]=pittke.coef[89,2]*(max(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))

sp.perform.trait.df.2[3,40]=pseuin.coef[147,2]*(mean(pseuin.traits$rmf)*mean(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,41]=pseuin.coef[147,2]*(min(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,42]=pseuin.coef[147,2]*(max(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,43]=pseuin.coef[147,2]*(min(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,44]=pseuin.coef[147,2]*(max(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))

colnames(sp.perform.trait.df.2)[40:44]=c("rmf.light.mean","rmf.light.min","rmf.light.max","rmf.light.min.max","rmf.light.max.min")

sp.perform.trait.df.2[1,45]=parach.coef[117,2]*(mean(parach.traits$rmf)*exp(mean(parach.traits$Comp1)))
sp.perform.trait.df.2[1,46]=parach.coef[117,2]*(min(parach.traits$rmf)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,47]=parach.coef[117,2]*(max(parach.traits$rmf)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,48]=parach.coef[117,2]*(min(parach.traits$rmf)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,49]=parach.coef[117,2]*(max(parach.traits$rmf)*exp(min(parach.traits$Comp1)))

sp.perform.trait.df.2[2,45]=pittke.coef[90,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,46]=pittke.coef[90,2]*(min(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,47]=pittke.coef[90,2]*(max(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,48]=pittke.coef[90,2]*(min(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,49]=pittke.coef[90,2]*(max(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))

sp.perform.trait.df.2[3,45]=pseuin.coef[148,2]*(mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,46]=pseuin.coef[148,2]*(min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,47]=pseuin.coef[148,2]*(max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,48]=pseuin.coef[148,2]*(min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,49]=pseuin.coef[148,2]*(max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))

colnames(sp.perform.trait.df.2)[45:49]=c("rmf.Comp1.mean","rmf.Comp1.min","rmf.Comp1.max","rmf.Comp1.min.max","rmf.Comp1.max.min")

sp.perform.trait.df.2[1,50]=parach.coef[118,2]*(mean(parach.traits$rmf)*exp(mean(parach.traits$Comp2)))
sp.perform.trait.df.2[1,51]=parach.coef[118,2]*(min(parach.traits$rmf)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,52]=parach.coef[118,2]*(max(parach.traits$rmf)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,53]=parach.coef[118,2]*(min(parach.traits$rmf)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,54]=parach.coef[118,2]*(max(parach.traits$rmf)*exp(min(parach.traits$Comp2)))

sp.perform.trait.df.2[2,50]=pittke.coef[91,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,51]=pittke.coef[91,2]*(min(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,52]=pittke.coef[91,2]*(max(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,53]=pittke.coef[91,2]*(min(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,54]=pittke.coef[91,2]*(max(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))

sp.perform.trait.df.2[3,50]=pseuin.coef[149,2]*(mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,51]=pseuin.coef[149,2]*(min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,52]=pseuin.coef[149,2]*(max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,53]=pseuin.coef[149,2]*(min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,54]=pseuin.coef[149,2]*(max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))

colnames(sp.perform.trait.df.2)[50:54]=c("rmf.Comp2.mean","rmf.Comp2.min","rmf.Comp2.max","rmf.Comp2.min.max","rmf.Comp2.max.min")

# Three-way interactions

sp.perform.trait.df.2[1,55]=parach.coef[119,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*mean(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,56]=parach.coef[119,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,57]=parach.coef[119,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,58]=parach.coef[119,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,59]=parach.coef[119,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*max(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,60]=parach.coef[119,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,61]=parach.coef[119,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,62]=parach.coef[119,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*min(parach.traits$perc.canopy.open))
sp.perform.trait.df.2[1,63]=parach.coef[119,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*max(parach.traits$perc.canopy.open))

sp.perform.trait.df.2[2,55]=pittke.coef[92,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,56]=pittke.coef[92,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,57]=pittke.coef[92,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,58]=pittke.coef[92,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,59]=pittke.coef[92,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,60]=pittke.coef[92,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,61]=pittke.coef[92,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,62]=pittke.coef[92,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*min(pittke.traits$perc.canopy.open))
sp.perform.trait.df.2[2,63]=pittke.coef[92,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*max(pittke.traits$perc.canopy.open))

sp.perform.trait.df.2[3,55]=pseuin.coef[150,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*mean(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,56]=pseuin.coef[150,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,57]=pseuin.coef[150,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,58]=pseuin.coef[150,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,59]=pseuin.coef[150,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,60]=pseuin.coef[150,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,61]=pseuin.coef[150,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,62]=pseuin.coef[150,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*min(pseuin.traits$perc.canopy.open))
sp.perform.trait.df.2[3,63]=pseuin.coef[150,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*max(pseuin.traits$perc.canopy.open))

colnames(sp.perform.trait.df.2)[55:63]=c("lma.rmf.light.mean","lma.rmf.light.min","lma.rmf.light.max",
                                         "lma.rmf.light.min.min.max","lma.rmf.light.min.max.max","lma.rmf.light.min.max.min",
                                         "lma.rmf.light.max.min.min","lma.rmf.light.max.max.min","lma.rmf.light.max.min.max")

sp.perform.trait.df.2[1,64]=parach.coef[120,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*exp(mean(parach.traits$Comp1)))
sp.perform.trait.df.2[1,65]=parach.coef[120,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,66]=parach.coef[120,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,67]=parach.coef[120,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,68]=parach.coef[120,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*exp(max(parach.traits$Comp1)))
sp.perform.trait.df.2[1,69]=parach.coef[120,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,70]=parach.coef[120,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,71]=parach.coef[120,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*exp(min(parach.traits$Comp1)))
sp.perform.trait.df.2[1,72]=parach.coef[120,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*exp(max(parach.traits$Comp1)))

sp.perform.trait.df.2[2,64]=pittke.coef[93,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,65]=pittke.coef[93,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,66]=pittke.coef[93,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,67]=pittke.coef[93,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,68]=pittke.coef[93,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,69]=pittke.coef[93,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,70]=pittke.coef[93,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,71]=pittke.coef[93,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*exp(min(pittke.traits$Comp1)))
sp.perform.trait.df.2[2,72]=pittke.coef[93,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*exp(max(pittke.traits$Comp1)))

sp.perform.trait.df.2[3,64]=pseuin.coef[151,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,65]=pseuin.coef[151,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,66]=pseuin.coef[151,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,67]=pseuin.coef[151,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,68]=pseuin.coef[151,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,69]=pseuin.coef[151,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,70]=pseuin.coef[151,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,71]=pseuin.coef[151,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp1)))
sp.perform.trait.df.2[3,72]=pseuin.coef[151,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp1)))

colnames(sp.perform.trait.df.2)[64:72]=c("lma.rmf.Comp1.mean","lma.rmf.Comp1.min","lma.rmf.Comp1.max",
                                         "lma.rmf.Comp1.min.min.max","lma.rmf.Comp1.min.max.max","lma.rmf.Comp1.min.max.min",
                                         "lma.rmf.Comp1.max.min.min","lma.rmf.Comp1.max.max.min","lma.rmf.Comp1.max.min.max")

sp.perform.trait.df.2[1,73]=parach.coef[121,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*exp(mean(parach.traits$Comp2)))
sp.perform.trait.df.2[1,74]=parach.coef[121,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,75]=parach.coef[121,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,76]=parach.coef[121,2]*(min(parach.traits$lma)*min(parach.traits$rmf)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,77]=parach.coef[121,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*exp(max(parach.traits$Comp2)))
sp.perform.trait.df.2[1,78]=parach.coef[121,2]*(min(parach.traits$lma)*max(parach.traits$rmf)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,79]=parach.coef[121,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,80]=parach.coef[121,2]*(max(parach.traits$lma)*max(parach.traits$rmf)*exp(min(parach.traits$Comp2)))
sp.perform.trait.df.2[1,81]=parach.coef[121,2]*(max(parach.traits$lma)*min(parach.traits$rmf)*exp(max(parach.traits$Comp2)))

sp.perform.trait.df.2[2,73]=pittke.coef[94,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,74]=pittke.coef[94,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,75]=pittke.coef[94,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,76]=pittke.coef[94,2]*(min(pittke.traits$lma)*min(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,77]=pittke.coef[94,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,78]=pittke.coef[94,2]*(min(pittke.traits$lma)*max(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,79]=pittke.coef[94,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,80]=pittke.coef[94,2]*(max(pittke.traits$lma)*max(pittke.traits$rmf)*exp(min(pittke.traits$Comp2)))
sp.perform.trait.df.2[2,81]=pittke.coef[94,2]*(max(pittke.traits$lma)*min(pittke.traits$rmf)*exp(max(pittke.traits$Comp2)))

sp.perform.trait.df.2[3,73]=pseuin.coef[152,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,74]=pseuin.coef[152,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,75]=pseuin.coef[152,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,76]=pseuin.coef[152,2]*(min(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,77]=pseuin.coef[152,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,78]=pseuin.coef[152,2]*(min(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,79]=pseuin.coef[152,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,80]=pseuin.coef[152,2]*(max(pseuin.traits$lma)*max(pseuin.traits$rmf)*exp(min(pseuin.traits$Comp2)))
sp.perform.trait.df.2[3,81]=pseuin.coef[152,2]*(max(pseuin.traits$lma)*min(pseuin.traits$rmf)*exp(max(pseuin.traits$Comp2)))

colnames(sp.perform.trait.df.2)[73:81]=c("lma.rmf.Comp2.mean","lma.rmf.Comp2.min","lma.rmf.Comp2.max",
                                         "lma.rmf.Comp2.min.min.max","lma.rmf.Comp2.min.max.max","lma.rmf.Comp2.min.max.min",
                                         "lma.rmf.Comp2.max.min.min","lma.rmf.Comp2.max.max.min","lma.rmf.Comp2.max.min.max")

# Growth performance of species considering all variables

sp.perform.trait.df.2[2,2]=pittke.coef[79,2]*mean(pittke.traits$lma)+
  pittke.coef[80,2]*mean(pittke.traits$rmf)+
  pittke.coef[82,2]*mean(pittke.traits$perc.canopy.open)+
  pittke.coef[83,2]*exp(mean(pittke.traits$Comp1))+
  pittke.coef[84,2]*exp(mean(pittke.traits$Comp2))+
  pittke.coef[85,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf))+
  pittke.coef[86,2]*(mean(pittke.traits$lma)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[87,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[88,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[89,2]*(mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[90,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[91,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[92,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[93,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[94,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))

sp.perform.trait.df.2[1,2]=parach.coef[106,2]*mean(parach.traits$lma)+
  parach.coef[107,2]*mean(parach.traits$rmf)+
  parach.coef[109,2]*mean(parach.traits$perc.canopy.open)+
  parach.coef[110,2]*exp(mean(parach.traits$Comp1))+
  parach.coef[111,2]*exp(mean(parach.traits$Comp2))+
  parach.coef[112,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf))+
  parach.coef[113,2]*(mean(parach.traits$lma)*mean(parach.traits$perc.canopy.open))+
  parach.coef[114,2]*(mean(parach.traits$lma)*exp(mean(parach.traits$Comp1)))+
  parach.coef[115,2]*(mean(parach.traits$lma)*exp(mean(parach.traits$Comp2)))+
  parach.coef[116,2]*(mean(parach.traits$rmf)*mean(parach.traits$perc.canopy.open))+
  parach.coef[117,2]*(mean(parach.traits$rmf)*exp(mean(parach.traits$Comp1)))+
  parach.coef[118,2]*(mean(parach.traits$rmf)*exp(mean(parach.traits$Comp2)))+
  parach.coef[119,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*mean(parach.traits$perc.canopy.open))+
  parach.coef[120,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*exp(mean(parach.traits$Comp1)))+
  parach.coef[121,2]*(mean(parach.traits$lma)*mean(parach.traits$rmf)*exp(mean(parach.traits$Comp2)))

sp.perform.trait.df.2[3,2]=pseuin.coef[137,2]*mean(pseuin.traits$lma)+
  pseuin.coef[138,2]*mean(pseuin.traits$rmf)+
  pseuin.coef[140,2]*mean(pseuin.traits$perc.canopy.open)+
  pseuin.coef[141,2]*exp(mean(pseuin.traits$Comp1))+
  pseuin.coef[142,2]*exp(mean(pseuin.traits$Comp2))+
  pseuin.coef[143,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf))+
  pseuin.coef[144,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$perc.canopy.open))+
  pseuin.coef[145,2]*(mean(pseuin.traits$lma)*exp(mean(pseuin.traits$Comp1)))+
  pseuin.coef[146,2]*(mean(pseuin.traits$lma)*exp(mean(pseuin.traits$Comp2)))+
  pseuin.coef[147,2]*(mean(pseuin.traits$rmf)*mean(pseuin.traits$perc.canopy.open))+
  pseuin.coef[148,2]*(mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp1)))+
  pseuin.coef[149,2]*(mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp2)))+
  pseuin.coef[150,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*mean(pseuin.traits$perc.canopy.open))+
  pseuin.coef[151,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp1)))+
  pseuin.coef[152,2]*(mean(pseuin.traits$lma)*mean(pseuin.traits$rmf)*exp(mean(pseuin.traits$Comp2)))

sp.perform.trait.df.2=t(sp.perform.trait.df.2)

write.csv(sp.perform.trait.df.2, file="arnold.results.csv")
