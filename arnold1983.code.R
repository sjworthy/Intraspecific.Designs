library(picante)
library(rethinking)
library(rstan)

# Read in the coefficient tables for each model
setwd("~/Desktop/Intrasp.perform.land/intra.models/Coeff.tables")

alphmo.coef=read.csv("alphmo.coeff.table.csv",header=T)
alsean.coef=read.csv("alsean.coeff.table.csv",header=T)
alsepe.coef=read.csv("alsepe.coeff.table.csv",header=T)
amoodu.coef=read.csv("amoodu.coeff.table.csv",header=T)
amooyu.coef=read.csv("amooyu.coeff.table.csv",header=T)
baccra.coef=read.csv("baccra.coeff.table.csv",header=T)
cappfo.coef=read.csv("cappfo.coeff.table.csv",header=T)
dichge.coef=read.csv("dichge.coeff.table.csv",header=T)
diosha.coef=read.csv("diosha.coeff.table.csv",header=T)
diosxi.coef=read.csv("diosxi.coeff.table.csv",header=T)
drypho.coef=read.csv("drypho.coeff.table.csv",header=T)
garcco.coef=read.csv("garcco.coeff.table.csv",header=T)
garufl.coef=read.csv("garufl.coeff.table.csv",header=T)
harpcu.coef=read.csv("harpcu.coeff.table.csv",header=T)
knemfu.coef=read.csv("knemfu.coeff.table.csv",header=T)
lindme.coef=read.csv("lindme.coeff.table.csv",header=T)
parach.coef=read.csv("parach.coeff.table.csv",header=T)
pittke.coef=read.csv("pittke.coeff.table.csv",header=T)
pometo.coef=read.csv("pometo.coeff.table.csv",header=T)
pseuin.coef=read.csv("pseuin.coeff.table.csv",header=T)
pterme.coef=read.csv("pterme.coeff.table.csv",header=T)
saprte.coef=read.csv("saprte.coeff.table.csv",header=T)
syzyla.coef=read.csv("syzyla.coeff.table.csv",header=T)
walsyu.coef=read.csv("walsyu.coeff.table.csv",header=T)

# Read in the trait data for each model

setwd("~/Desktop/Intrasp.perform.land/Outputs")

alphmo.traits=read.csv("alphmo.csv",header=T)
alsean.traits=read.csv("alsean.csv",header=T)
alsepe.traits=read.csv("alsepe.csv",header=T)
amoodu.traits=read.csv("amoodu.csv",header=T)
amooyu.traits=read.csv("amooyu.csv",header=T)
baccra.traits=read.csv("baccra.csv",header=T)
cappfo.traits=read.csv("cappfo.csv",header=T)
dichge.traits=read.csv("dichge.csv",header=T)
diosha.traits=read.csv("diosha.csv",header=T)
diosxi.traits=read.csv("diosxi.csv",header=T)
drypho.traits=read.csv("drypho.csv",header=T)
garcco.traits=read.csv("garcco.csv",header=T)
garufl.traits=read.csv("garufl.csv",header=T)
harpcu.traits=read.csv("harpcu.csv",header=T)
knemfu.traits=read.csv("knemfu.csv",header=T)
lindme.traits=read.csv("lindme.csv",header=T)
parach.traits=read.csv("parach.csv",header=T)
pittke.traits=read.csv("pittke.csv",header=T)
pometo.traits=read.csv("pometo.csv",header=T)
pseuin.traits=read.csv("pseuin.csv",header=T)
pterme.traits=read.csv("pterme.csv",header=T)
saprte.traits=read.csv("saprte.csv",header=T)
syzyla.traits=read.csv("syzyla.csv",header=T)
walsyu.traits=read.csv("walysu.csv",header=T)

# Growth performance of species based on contribution of each parameter
# Arnold 1983
# General equation form is beta of variable from coeff.table x mean value 
# of that trait(s) for that species + same thing for all variables

sp.perform.trait.df=matrix(data=NA, nrow=24,ncol=2)
colnames(sp.perform.trait.df)=c("traits.only", "all.parameters")
row.names(sp.perform.trait.df)=c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
                                 "dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
                                 "lindme", "parach", "pittke", "pometo", "pseuin","pterme", "saprte", "syzyla", "walsyu")
sp.perform.trait.df.2=as.data.frame(sp.perform.trait.df)
colnames(sp.perform.trait.df.2)[3:9]=c("lma","rmf","thick","ssl","lar","lmf","smf")
colnames(sp.perform.trait.df.2)[10:16]=c("lma.min","rmf.min","thick.min","ssl.min","lar.min","lmf.min","smf.min")
colnames(sp.perform.trait.df.2)[17:23]=c("lma.max","rmf.max","thick.max","ssl.max","lar.max","lmf.max","smf.max")
colnames(sp.perform.trait.df.2)[24:26]=c("light.mean","pc1.mean","pc2.mean")
colnames(sp.perform.trait.df.2)[27:29]=c("light.min","pc1.min","pc2.min")
colnames(sp.perform.trait.df.2)[30:32]=c("light.max","pc1.max","pc2.max")
colnames(sp.perform.trait.df.2)[33:51]=c("lma.smf.mean","lma.rmf.mean","lma.light.mean","lma.pc1.mean",
                                         "lma.pc2.mean","lmf.lar.mean","lmf.pc2.mean","smf.thick.mean",
                                         "smf.light.mean","smf.pc1.mean","rmf.thick.mean","rmf.thick.mean",
                                         "rmf.pc1.mean","rmf.pc2.mean","ssl.thick.mean","ssl.pc2.mean",
                                         "thick.pc1.mean","thick.pc2.mean", "lar.pc2.mean")

# Growth performance of species for each trait separately to see which one
# has the largest contribution to growth for each species
# mean, min, and max trait value used

sp.perform.trait.df.2[1,3]=alphmo.coef[18,2]*mean(alphmo.traits$lma)
sp.perform.trait.df.2[1,4]=alphmo.coef[19,2]*mean(alphmo.traits$rmf)
sp.perform.trait.df.2[1,5]=alphmo.coef[20,2]*mean(alphmo.traits$mean.thick)
sp.perform.trait.df.2[1,6]=alphmo.coef[21,2]*mean(alphmo.traits$ssl)
sp.perform.trait.df.2[1,7]=alphmo.coef[22,2]*mean(alphmo.traits$lar1)
sp.perform.trait.df.2[1,8]=alphmo.coef[23,2]*mean(alphmo.traits$lmf)
sp.perform.trait.df.2[1,9]=alphmo.coef[24,2]*mean(alphmo.traits$smf)

sp.perform.trait.df.2[2,3]=alsean.coef[6,2]*mean(alsean.traits$lma)
sp.perform.trait.df.2[2,4]=alsean.coef[7,2]*mean(alsean.traits$rmf)
sp.perform.trait.df.2[2,5]=alsean.coef[8,2]*mean(alsean.traits$mean.thick)
sp.perform.trait.df.2[2,6]=alsean.coef[9,2]*mean(alsean.traits$ssl)
sp.perform.trait.df.2[2,7]=alsean.coef[10,2]*mean(alsean.traits$lar1)
sp.perform.trait.df.2[2,8]=alsean.coef[11,2]*mean(alsean.traits$lmf)
sp.perform.trait.df.2[2,9]=alsean.coef[12,2]*mean(alsean.traits$smf)

sp.perform.trait.df.2[3,3]=alsepe.coef[6,2]*mean(alsepe.traits$lma)
sp.perform.trait.df.2[3,4]=alsepe.coef[7,2]*mean(alsepe.traits$rmf)
sp.perform.trait.df.2[3,5]=alsepe.coef[8,2]*mean(alsepe.traits$mean.thick)
sp.perform.trait.df.2[3,6]=alsepe.coef[9,2]*mean(alsepe.traits$ssl)
sp.perform.trait.df.2[3,7]=alsepe.coef[10,2]*mean(alsepe.traits$lar1)
sp.perform.trait.df.2[3,8]=alsepe.coef[11,2]*mean(alsepe.traits$lmf)
sp.perform.trait.df.2[3,9]=alsepe.coef[12,2]*mean(alsepe.traits$smf)

sp.perform.trait.df.2[4,3]=amoodu.coef[39,2]*mean(amoodu.traits$lma)
sp.perform.trait.df.2[4,4]=amoodu.coef[40,2]*mean(amoodu.traits$rmf)
sp.perform.trait.df.2[4,5]=amoodu.coef[41,2]*mean(amoodu.traits$mean.thick)
sp.perform.trait.df.2[4,6]=amoodu.coef[42,2]*mean(amoodu.traits$ssl)
sp.perform.trait.df.2[4,7]=amoodu.coef[43,2]*mean(amoodu.traits$lar1)
sp.perform.trait.df.2[4,8]=amoodu.coef[44,2]*mean(amoodu.traits$lmf)
sp.perform.trait.df.2[4,9]=amoodu.coef[45,2]*mean(amoodu.traits$smf)

sp.perform.trait.df.2[5,3]=amooyu.coef[24,2]*mean(amooyu.traits$lma)
sp.perform.trait.df.2[5,4]=amooyu.coef[25,2]*mean(amooyu.traits$rmf)
sp.perform.trait.df.2[5,5]=amooyu.coef[26,2]*mean(amooyu.traits$mean.thick)
sp.perform.trait.df.2[5,6]=amooyu.coef[27,2]*mean(amooyu.traits$ssl)
sp.perform.trait.df.2[5,7]=amooyu.coef[28,2]*mean(amooyu.traits$lar1)
sp.perform.trait.df.2[5,8]=amooyu.coef[29,2]*mean(amooyu.traits$lmf)
sp.perform.trait.df.2[5,9]=amooyu.coef[30,2]*mean(amooyu.traits$smf)

sp.perform.trait.df.2[6,3]=baccra.coef[12,2]*mean(baccra.traits$lma)
sp.perform.trait.df.2[6,4]=baccra.coef[13,2]*mean(baccra.traits$rmf)
sp.perform.trait.df.2[6,5]=baccra.coef[14,2]*mean(baccra.traits$mean.thick)
sp.perform.trait.df.2[6,6]=baccra.coef[15,2]*mean(baccra.traits$ssl)
sp.perform.trait.df.2[6,7]=baccra.coef[16,2]*mean(baccra.traits$lar1)
sp.perform.trait.df.2[6,8]=baccra.coef[17,2]*mean(baccra.traits$lmf)
sp.perform.trait.df.2[6,9]=baccra.coef[18,2]*mean(baccra.traits$smf)

sp.perform.trait.df.2[7,3]=cappfo.coef[27,2]*mean(cappfo.traits$lma)
sp.perform.trait.df.2[7,4]=cappfo.coef[28,2]*mean(cappfo.traits$rmf)
sp.perform.trait.df.2[7,5]=cappfo.coef[29,2]*mean(cappfo.traits$mean.thick)
sp.perform.trait.df.2[7,6]=cappfo.coef[30,2]*mean(cappfo.traits$ssl)
sp.perform.trait.df.2[7,7]=cappfo.coef[31,2]*mean(cappfo.traits$lar1)
sp.perform.trait.df.2[7,8]=cappfo.coef[32,2]*mean(cappfo.traits$lmf)
sp.perform.trait.df.2[7,9]=cappfo.coef[33,2]*mean(cappfo.traits$smf)

sp.perform.trait.df.2[8,3]=dichge.coef[33,2]*mean(dichge.traits$lma)
sp.perform.trait.df.2[8,4]=dichge.coef[34,2]*mean(dichge.traits$rmf)
sp.perform.trait.df.2[8,5]=dichge.coef[35,2]*mean(dichge.traits$mean.thick)
sp.perform.trait.df.2[8,6]=dichge.coef[36,2]*mean(dichge.traits$ssl)
sp.perform.trait.df.2[8,7]=dichge.coef[37,2]*mean(dichge.traits$lar1)
sp.perform.trait.df.2[8,8]=dichge.coef[38,2]*mean(dichge.traits$lmf)
sp.perform.trait.df.2[8,9]=dichge.coef[39,2]*mean(dichge.traits$smf)

sp.perform.trait.df.2[9,3]=diosha.coef[23,2]*mean(diosha.traits$lma)
sp.perform.trait.df.2[9,4]=diosha.coef[24,2]*mean(diosha.traits$rmf)
sp.perform.trait.df.2[9,5]=diosha.coef[25,2]*mean(diosha.traits$mean.thick)
sp.perform.trait.df.2[9,6]=diosha.coef[26,2]*mean(diosha.traits$ssl)
sp.perform.trait.df.2[9,7]=diosha.coef[27,2]*mean(diosha.traits$lar1)
sp.perform.trait.df.2[9,8]=diosha.coef[28,2]*mean(diosha.traits$lmf)
sp.perform.trait.df.2[9,9]=diosha.coef[29,2]*mean(diosha.traits$smf)

sp.perform.trait.df.2[10,3]=diosxi.coef[32,2]*mean(diosxi.traits$lma)
sp.perform.trait.df.2[10,4]=diosxi.coef[33,2]*mean(diosxi.traits$rmf)
sp.perform.trait.df.2[10,5]=diosxi.coef[34,2]*mean(diosxi.traits$mean.thick)
sp.perform.trait.df.2[10,6]=diosxi.coef[35,2]*mean(diosxi.traits$ssl)
sp.perform.trait.df.2[10,7]=diosxi.coef[36,2]*mean(diosxi.traits$lar1)
sp.perform.trait.df.2[10,8]=diosxi.coef[37,2]*mean(diosxi.traits$lmf)
sp.perform.trait.df.2[10,9]=diosxi.coef[38,2]*mean(diosxi.traits$smf)

sp.perform.trait.df.2[11,3]=drypho.coef[16,2]*mean(drypho.traits$lma)
sp.perform.trait.df.2[11,4]=drypho.coef[17,2]*mean(drypho.traits$rmf)
sp.perform.trait.df.2[11,5]=drypho.coef[18,2]*mean(drypho.traits$mean.thick)
sp.perform.trait.df.2[11,6]=drypho.coef[19,2]*mean(drypho.traits$ssl)
sp.perform.trait.df.2[11,7]=drypho.coef[20,2]*mean(drypho.traits$lar1)
sp.perform.trait.df.2[11,8]=drypho.coef[21,2]*mean(drypho.traits$lmf)
sp.perform.trait.df.2[11,9]=drypho.coef[22,2]*mean(drypho.traits$smf)

sp.perform.trait.df.2[12,3]=garcco.coef[19,2]*mean(garcco.traits$lma)
sp.perform.trait.df.2[12,4]=garcco.coef[20,2]*mean(garcco.traits$rmf)
sp.perform.trait.df.2[12,5]=garcco.coef[21,2]*mean(garcco.traits$mean.thick)
sp.perform.trait.df.2[12,6]=garcco.coef[22,2]*mean(garcco.traits$ssl)
sp.perform.trait.df.2[12,7]=garcco.coef[23,2]*mean(garcco.traits$lar1)
sp.perform.trait.df.2[12,8]=garcco.coef[24,2]*mean(garcco.traits$lmf)
sp.perform.trait.df.2[12,9]=garcco.coef[25,2]*mean(garcco.traits$smf)

sp.perform.trait.df.2[13,3]=garufl.coef[5,2]*mean(garufl.traits$lma)
sp.perform.trait.df.2[13,4]=garufl.coef[6,2]*mean(garufl.traits$rmf)
sp.perform.trait.df.2[13,5]=garufl.coef[7,2]*mean(garufl.traits$mean.thick)
sp.perform.trait.df.2[13,6]=garufl.coef[8,2]*mean(garufl.traits$ssl)
sp.perform.trait.df.2[13,7]=garufl.coef[9,2]*mean(garufl.traits$lar1)
sp.perform.trait.df.2[13,8]=garufl.coef[10,2]*mean(garufl.traits$lmf)
sp.perform.trait.df.2[13,9]=garufl.coef[11,2]*mean(garufl.traits$smf)

sp.perform.trait.df.2[14,3]=harpcu.coef[11,2]*mean(harpcu.traits$lma)
sp.perform.trait.df.2[14,4]=harpcu.coef[12,2]*mean(harpcu.traits$rmf)
sp.perform.trait.df.2[14,5]=harpcu.coef[13,2]*mean(harpcu.traits$mean.thick)
sp.perform.trait.df.2[14,6]=harpcu.coef[14,2]*mean(harpcu.traits$ssl)
sp.perform.trait.df.2[14,7]=harpcu.coef[15,2]*mean(harpcu.traits$lar1)
sp.perform.trait.df.2[14,8]=harpcu.coef[16,2]*mean(harpcu.traits$lmf)
sp.perform.trait.df.2[14,9]=harpcu.coef[17,2]*mean(harpcu.traits$smf)

sp.perform.trait.df.2[15,3]=knemfu.coef[18,2]*mean(knemfu.traits$lma)
sp.perform.trait.df.2[15,4]=knemfu.coef[19,2]*mean(knemfu.traits$rmf)
sp.perform.trait.df.2[15,5]=knemfu.coef[20,2]*mean(knemfu.traits$mean.thick)
sp.perform.trait.df.2[15,6]=knemfu.coef[21,2]*mean(knemfu.traits$ssl)
sp.perform.trait.df.2[15,7]=knemfu.coef[22,2]*mean(knemfu.traits$lar1)
sp.perform.trait.df.2[15,8]=knemfu.coef[23,2]*mean(knemfu.traits$lmf)
sp.perform.trait.df.2[15,9]=knemfu.coef[24,2]*mean(knemfu.traits$smf)

sp.perform.trait.df.2[16,3]=lindme.coef[18,2]*mean(lindme.traits$lma)
sp.perform.trait.df.2[16,4]=lindme.coef[19,2]*mean(lindme.traits$rmf)
sp.perform.trait.df.2[16,5]=lindme.coef[20,2]*mean(lindme.traits$mean.thick)
sp.perform.trait.df.2[16,6]=lindme.coef[21,2]*mean(lindme.traits$ssl)
sp.perform.trait.df.2[16,7]=lindme.coef[22,2]*mean(lindme.traits$lar1)
sp.perform.trait.df.2[16,8]=lindme.coef[23,2]*mean(lindme.traits$lmf)
sp.perform.trait.df.2[16,9]=lindme.coef[24,2]*mean(lindme.traits$smf)

sp.perform.trait.df.2[17,3]=parach.coef[106,2]*mean(parach.traits$lma)
sp.perform.trait.df.2[17,4]=parach.coef[107,2]*mean(parach.traits$rmf)
sp.perform.trait.df.2[17,5]=parach.coef[108,2]*mean(parach.traits$mean.thick)
sp.perform.trait.df.2[17,6]=parach.coef[109,2]*mean(parach.traits$ssl)
sp.perform.trait.df.2[17,7]=parach.coef[110,2]*mean(parach.traits$lar1)
sp.perform.trait.df.2[17,8]=parach.coef[111,2]*mean(parach.traits$lmf)
sp.perform.trait.df.2[17,9]=parach.coef[112,2]*mean(parach.traits$smf)

sp.perform.trait.df.2[18,3]=pittke.coef[79,2]*mean(pittke.traits$lma)
sp.perform.trait.df.2[18,4]=pittke.coef[80,2]*mean(pittke.traits$rmf)
sp.perform.trait.df.2[18,5]=pittke.coef[81,2]*mean(pittke.traits$mean.thick)
sp.perform.trait.df.2[18,6]=pittke.coef[82,2]*mean(pittke.traits$ssl)
sp.perform.trait.df.2[18,7]=pittke.coef[83,2]*mean(pittke.traits$lar1)
sp.perform.trait.df.2[18,8]=pittke.coef[84,2]*mean(pittke.traits$lmf)
sp.perform.trait.df.2[18,9]=pittke.coef[85,2]*mean(pittke.traits$smf)

sp.perform.trait.df.2[19,3]=pometo.coef[25,2]*mean(pometo.traits$lma)
sp.perform.trait.df.2[19,4]=pometo.coef[26,2]*mean(pometo.traits$rmf)
sp.perform.trait.df.2[19,5]=pometo.coef[27,2]*mean(pometo.traits$mean.thick)
sp.perform.trait.df.2[19,6]=pometo.coef[28,2]*mean(pometo.traits$ssl)
sp.perform.trait.df.2[19,7]=pometo.coef[29,2]*mean(pometo.traits$lar1)
sp.perform.trait.df.2[19,8]=pometo.coef[30,2]*mean(pometo.traits$lmf)
sp.perform.trait.df.2[19,9]=pometo.coef[31,2]*mean(pometo.traits$smf)

sp.perform.trait.df.2[20,3]=pseuin.coef[137,2]*mean(pseuin.traits$lma)
sp.perform.trait.df.2[20,4]=pseuin.coef[138,2]*mean(pseuin.traits$rmf)
sp.perform.trait.df.2[20,5]=pseuin.coef[139,2]*mean(pseuin.traits$mean.thick)
sp.perform.trait.df.2[20,6]=pseuin.coef[140,2]*mean(pseuin.traits$ssl)
sp.perform.trait.df.2[20,7]=pseuin.coef[141,2]*mean(pseuin.traits$lar1)
sp.perform.trait.df.2[20,8]=pseuin.coef[142,2]*mean(pseuin.traits$lmf)
sp.perform.trait.df.2[20,9]=pseuin.coef[143,2]*mean(pseuin.traits$smf)

sp.perform.trait.df.2[21,3]=pterme.coef[12,2]*mean(pterme.traits$lma)
sp.perform.trait.df.2[21,4]=pterme.coef[13,2]*mean(pterme.traits$rmf)
sp.perform.trait.df.2[21,5]=pterme.coef[14,2]*mean(pterme.traits$mean.thick)
sp.perform.trait.df.2[21,6]=pterme.coef[15,2]*mean(pterme.traits$ssl)
sp.perform.trait.df.2[21,7]=pterme.coef[16,2]*mean(pterme.traits$lar1)
sp.perform.trait.df.2[21,8]=pterme.coef[17,2]*mean(pterme.traits$lmf)
sp.perform.trait.df.2[21,9]=pterme.coef[18,2]*mean(pterme.traits$smf)

sp.perform.trait.df.2[22,3]=saprte.coef[39,2]*mean(saprte.traits$lma)
sp.perform.trait.df.2[22,4]=saprte.coef[40,2]*mean(saprte.traits$rmf)
sp.perform.trait.df.2[22,5]=saprte.coef[41,2]*mean(saprte.traits$mean.thick)
sp.perform.trait.df.2[22,6]=saprte.coef[42,2]*mean(saprte.traits$ssl)
sp.perform.trait.df.2[22,7]=saprte.coef[43,2]*mean(saprte.traits$lar1)
sp.perform.trait.df.2[22,8]=saprte.coef[44,2]*mean(saprte.traits$lmf)
sp.perform.trait.df.2[22,9]=saprte.coef[45,2]*mean(saprte.traits$smf)

sp.perform.trait.df.2[23,3]=syzyla.coef[11,2]*mean(syzyla.traits$lma)
sp.perform.trait.df.2[23,4]=syzyla.coef[12,2]*mean(syzyla.traits$rmf)
sp.perform.trait.df.2[23,5]=syzyla.coef[13,2]*mean(syzyla.traits$mean.thick)
sp.perform.trait.df.2[23,6]=syzyla.coef[14,2]*mean(syzyla.traits$ssl)
sp.perform.trait.df.2[23,7]=syzyla.coef[15,2]*mean(syzyla.traits$lar1)
sp.perform.trait.df.2[23,8]=syzyla.coef[16,2]*mean(syzyla.traits$lmf)
sp.perform.trait.df.2[23,9]=syzyla.coef[17,2]*mean(syzyla.traits$smf)

sp.perform.trait.df.2[24,3]=walsyu.coef[33,2]*mean(walsyu.traits$lma)
sp.perform.trait.df.2[24,4]=walsyu.coef[34,2]*mean(walsyu.traits$rmf)
sp.perform.trait.df.2[24,5]=walsyu.coef[35,2]*mean(walsyu.traits$mean.thick)
sp.perform.trait.df.2[24,6]=walsyu.coef[36,2]*mean(walsyu.traits$ssl)
sp.perform.trait.df.2[24,7]=walsyu.coef[37,2]*mean(walsyu.traits$lar1)
sp.perform.trait.df.2[24,8]=walsyu.coef[38,2]*mean(walsyu.traits$lmf)
sp.perform.trait.df.2[24,9]=walsyu.coef[39,2]*mean(walsyu.traits$smf)

# Example of calculation for 2-way interaction
sp.perform.trait.df.2[1,33]=alphmo.coef[29,2]*(mean(alphmo.traits$lma)*mean(alphmo.traits$smf))
sp.perform.trait.df.2[1,52]=alphmo.coef[29,2]*(min(alphmo.traits$lma)*min(alphmo.traits$smf))
sp.perform.trait.df.2[1,71]=alphmo.coef[29,2]*(max(alphmo.traits$lma)*max(alphmo.traits$smf))
sp.perform.trait.df.2[1,71]=alphmo.coef[29,2]*(min(alphmo.traits$lma)*max(alphmo.traits$smf))
sp.perform.trait.df.2[1,71]=alphmo.coef[29,2]*(max(alphmo.traits$lma)*min(alphmo.traits$smf))

sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(mean(saprte.traits$lma)*mean(saprte.traits$smf)*mean(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(min(saprte.traits$lma)*min(saprte.traits$smf)*min(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(max(saprte.traits$lma)*max(saprte.traits$smf)*max(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(min(saprte.traits$lma)*min(saprte.traits$smf)*max(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(min(saprte.traits$lma)*max(saprte.traits$smf)*max(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(min(saprte.traits$lma)*max(saprte.traits$smf)*min(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(max(saprte.traits$lma)*min(saprte.traits$smf)*min(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(max(saprte.traits$lma)*max(saprte.traits$smf)*min(saprte.traits$perc.canopy.open))
sp.perform.trait.df.2[23,33]=saprte.coef[44,2]*(max(saprte.traits$lma)*min(saprte.traits$smf)*max(saprte.traits$perc.canopy.open))


# Growth performance of species for all functional traits combined

alphmo.perform.trait=alphmo.coef[18,2]*mean(alphmo.traits$lma)+alphmo.coef[19,2]*mean(alphmo.traits$rmf)+
  alphmo.coef[20,2]*mean(alphmo.traits$mean.thick)+alphmo.coef[21,2]*mean(alphmo.traits$ssl)+
  alphmo.coef[22,2]*mean(alphmo.traits$lar1)+alphmo.coef[23,2]*mean(alphmo.traits$lmf)+
  alphmo.coef[24,2]*mean(alphmo.traits$smf)
alsean.perform.trait=alsean.coef[6,2]*mean(alsean.traits$lma)+alsean.coef[7,2]*mean(alsean.traits$rmf)+
  alsean.coef[8,2]*mean(alsean.traits$mean.thick)+alsean.coef[9,2]*mean(alsean.traits$ssl)+
  alsean.coef[10,2]*mean(alsean.traits$lar1)+alsean.coef[11,2]*mean(alsean.traits$lmf)+
  alsean.coef[12,2]*mean(alsean.traits$smf)
alsepe.perform.trait=alsepe.coef[6,2]*mean(alsepe.traits$lma)+alsepe.coef[7,2]*mean(alsepe.traits$rmf)+
  alsepe.coef[8,2]*mean(alsepe.traits$mean.thick)+alsepe.coef[9,2]*mean(alsepe.traits$ssl)+
  alsepe.coef[10,2]*mean(alsepe.traits$lar1)+alsepe.coef[11,2]*mean(alsepe.traits$lmf)+
  alsepe.coef[12,2]*mean(alsepe.traits$smf)
amoodu.perform.trait=amoodu.coef[39,2]*mean(amoodu.traits$lma)+amoodu.coef[40,2]*mean(amoodu.traits$rmf)+
  amoodu.coef[41,2]*mean(amoodu.traits$mean.thick)+amoodu.coef[42,2]*mean(amoodu.traits$ssl)+
  amoodu.coef[43,2]*mean(amoodu.traits$lar1)+amoodu.coef[44,2]*mean(amoodu.traits$lmf)+
  amoodu.coef[45,2]*mean(amoodu.traits$smf)
amooyu.perform.trait=amooyu.coef[24,2]*mean(amooyu.traits$lma)+amooyu.coef[25,2]*mean(amooyu.traits$rmf)+
  amooyu.coef[26,2]*mean(amooyu.traits$mean.thick)+amooyu.coef[27,2]*mean(amooyu.traits$ssl)+
  amooyu.coef[28,2]*mean(amooyu.traits$lar1)+amooyu.coef[29,2]*mean(amooyu.traits$lmf)+
  amooyu.coef[30,2]*mean(amooyu.traits$smf)
baccra.perform.trait=baccra.coef[12,2]*mean(baccra.traits$lma)+baccra.coef[13,2]*mean(baccra.traits$rmf)+
  baccra.coef[14,2]*mean(baccra.traits$mean.thick)+baccra.coef[15,2]*mean(baccra.traits$ssl)+
  baccra.coef[16,2]*mean(baccra.traits$lar1)+baccra.coef[17,2]*mean(baccra.traits$lmf)+
  baccra.coef[18,2]*mean(baccra.traits$smf)
cappfo.perform.trait=cappfo.coef[27,2]*mean(cappfo.traits$lma)+cappfo.coef[28,2]*mean(cappfo.traits$rmf)+
  cappfo.coef[29,2]*mean(cappfo.traits$mean.thick)+cappfo.coef[30,2]*mean(cappfo.traits$ssl)+
  cappfo.coef[31,2]*mean(cappfo.traits$lar1)+cappfo.coef[32,2]*mean(cappfo.traits$lmf)+
  cappfo.coef[33,2]*mean(cappfo.traits$smf)
dichge.perform.trait=dichge.coef[33,2]*mean(dichge.traits$lma)+dichge.coef[34,2]*mean(dichge.traits$rmf)+
  dichge.coef[35,2]*mean(dichge.traits$mean.thick)+dichge.coef[36,2]*mean(dichge.traits$ssl)+
  dichge.coef[37,2]*mean(dichge.traits$lar1)+dichge.coef[38,2]*mean(dichge.traits$lmf)+
  dichge.coef[39,2]*mean(dichge.traits$smf)
diosha.perform.trait=diosha.coef[23,2]*mean(diosha.traits$lma)+diosha.coef[24,2]*mean(diosha.traits$rmf)+
  diosha.coef[25,2]*mean(diosha.traits$mean.thick)+diosha.coef[26,2]*mean(diosha.traits$ssl)+
  diosha.coef[27,2]*mean(diosha.traits$lar1)+diosha.coef[28,2]*mean(diosha.traits$lmf)+
  diosha.coef[29,2]*mean(diosha.traits$smf)
diosxi.perform.trait=diosxi.coef[32,2]*mean(diosxi.traits$lma)+diosxi.coef[33,2]*mean(diosxi.traits$rmf)+
  diosxi.coef[34,2]*mean(diosxi.traits$mean.thick)+diosxi.coef[35,2]*mean(diosxi.traits$ssl)+
  diosxi.coef[36,2]*mean(diosxi.traits$lar1)+diosxi.coef[37,2]*mean(diosxi.traits$lmf)+
  diosxi.coef[38,2]*mean(diosxi.traits$smf)
drypho.perform.trait=drypho.coef[16,2]*mean(drypho.traits$lma)+drypho.coef[17,2]*mean(drypho.traits$rmf)+
  drypho.coef[18,2]*mean(drypho.traits$mean.thick)+drypho.coef[19,2]*mean(drypho.traits$ssl)+
  drypho.coef[20,2]*mean(drypho.traits$lar1)+drypho.coef[21,2]*mean(drypho.traits$lmf)+
  drypho.coef[22,2]*mean(drypho.traits$smf)
garcco.perform.trait=garcco.coef[19,2]*mean(garcco.traits$lma)+garcco.coef[20,2]*mean(garcco.traits$rmf)+
  garcco.coef[21,2]*mean(garcco.traits$mean.thick)+garcco.coef[22,2]*mean(garcco.traits$ssl)+
  garcco.coef[23,2]*mean(garcco.traits$lar1)+garcco.coef[24,2]*mean(garcco.traits$lmf)+
  garcco.coef[25,2]*mean(garcco.traits$smf)
garufl.perform.trait=garufl.coef[5,2]*mean(garufl.traits$lma)+garufl.coef[6,2]*mean(garufl.traits$rmf)+
  garufl.coef[7,2]*mean(garufl.traits$mean.thick)+garufl.coef[8,2]*mean(garufl.traits$ssl)+
  garufl.coef[9,2]*mean(garufl.traits$lar1)+garufl.coef[10,2]*mean(garufl.traits$lmf)+
  garufl.coef[11,2]*mean(garufl.traits$smf)
harpcu.perform.trait=harpcu.coef[11,2]*mean(harpcu.traits$lma)+harpcu.coef[12,2]*mean(harpcu.traits$rmf)+
  harpcu.coef[13,2]*mean(harpcu.traits$mean.thick)+harpcu.coef[14,2]*mean(harpcu.traits$ssl)+
  harpcu.coef[15,2]*mean(harpcu.traits$lar1)+harpcu.coef[16,2]*mean(harpcu.traits$lmf)+
  harpcu.coef[17,2]*mean(harpcu.traits$smf)
knemfu.perform.trait=knemfu.coef[18,2]*mean(knemfu.traits$lma)+knemfu.coef[19,2]*mean(knemfu.traits$rmf)+
  knemfu.coef[20,2]*mean(knemfu.traits$mean.thick)+knemfu.coef[21,2]*mean(knemfu.traits$ssl)+
  knemfu.coef[22,2]*mean(knemfu.traits$lar1)+knemfu.coef[23,2]*mean(knemfu.traits$lmf)+
  knemfu.coef[24,2]*mean(knemfu.traits$smf)
lindme.perform.trait=lindme.coef[18,2]*mean(lindme.traits$lma)+lindme.coef[19,2]*mean(lindme.traits$rmf)+
  lindme.coef[20,2]*mean(lindme.traits$mean.thick)+lindme.coef[21,2]*mean(lindme.traits$ssl)+
  lindme.coef[22,2]*mean(lindme.traits$lar1)+lindme.coef[23,2]*mean(lindme.traits$lmf)+
  lindme.coef[24,2]*mean(lindme.traits$smf)
parach.perform.trait=parach.coef[106,2]*mean(parach.traits$lma)+parach.coef[107,2]*mean(parach.traits$rmf)+
  parach.coef[108,2]*mean(parach.traits$mean.thick)+parach.coef[109,2]*mean(parach.traits$ssl)+
  parach.coef[110,2]*mean(parach.traits$lar1)+parach.coef[111,2]*mean(parach.traits$lmf)+
  parach.coef[112,2]*mean(parach.traits$smf)
pittke.perform.trait=pittke.coef[79,2]*mean(pittke.traits$lma)+pittke.coef[80,2]*mean(pittke.traits$rmf)+
  pittke.coef[81,2]*mean(pittke.traits$mean.thick)+pittke.coef[82,2]*mean(pittke.traits$ssl)+
  pittke.coef[83,2]*mean(pittke.traits$lar1)+pittke.coef[84,2]*mean(pittke.traits$lmf)+
  pittke.coef[85,2]*mean(pittke.traits$smf)
pometo.perform.trait=pometo.coef[25,2]*mean(pometo.traits$lma)+pometo.coef[26,2]*mean(pometo.traits$rmf)+
  pometo.coef[27,2]*mean(pometo.traits$mean.thick)+pometo.coef[28,2]*mean(pometo.traits$ssl)+
  pometo.coef[29,2]*mean(pometo.traits$lar1)+pometo.coef[30,2]*mean(pometo.traits$lmf)+
  pometo.coef[31,2]*mean(pometo.traits$smf)
pseuin.perform.trait=pseuin.coef[137,2]*mean(pseuin.traits$lma)+pseuin.coef[138,2]*mean(pseuin.traits$rmf)+
  pseuin.coef[139,2]*mean(pseuin.traits$mean.thick)+pseuin.coef[140,2]*mean(pseuin.traits$ssl)+
  pseuin.coef[141,2]*mean(pseuin.traits$lar1)+pseuin.coef[142,2]*mean(pseuin.traits$lmf)+
  pseuin.coef[143,2]*mean(pseuin.traits$smf)
pterme.perform.trait=pterme.coef[12,2]*mean(pterme.traits$lma)+pterme.coef[13,2]*mean(pterme.traits$rmf)+
  pterme.coef[14,2]*mean(pterme.traits$mean.thick)+pterme.coef[15,2]*mean(pterme.traits$ssl)+
  pterme.coef[16,2]*mean(pterme.traits$lar1)+pterme.coef[17,2]*mean(pterme.traits$lmf)+
  pterme.coef[18,2]*mean(pterme.traits$smf)
saprte.perform.trait=saprte.coef[39,2]*mean(saprte.traits$lma)+saprte.coef[40,2]*mean(saprte.traits$rmf)+
  saprte.coef[41,2]*mean(saprte.traits$mean.thick)+saprte.coef[42,2]*mean(saprte.traits$ssl)+
  saprte.coef[43,2]*mean(saprte.traits$lar1)+saprte.coef[44,2]*mean(saprte.traits$lmf)+
  saprte.coef[45,2]*mean(saprte.traits$smf)
syzyla.perform.trait=syzyla.coef[11,2]*mean(syzyla.traits$lma)+syzyla.coef[12,2]*mean(syzyla.traits$rmf)+
  syzyla.coef[13,2]*mean(syzyla.traits$mean.thick)+syzyla.coef[14,2]*mean(syzyla.traits$ssl)+
  syzyla.coef[15,2]*mean(syzyla.traits$lar1)+syzyla.coef[16,2]*mean(syzyla.traits$lmf)+
  syzyla.coef[17,2]*mean(syzyla.traits$smf)
walsyu.perform.trait=walsyu.coef[33,2]*mean(walsyu.traits$lma)+walsyu.coef[34,2]*mean(walsyu.traits$rmf)+
  walsyu.coef[35,2]*mean(walsyu.traits$mean.thick)+walsyu.coef[36,2]*mean(walsyu.traits$ssl)+
  walsyu.coef[37,2]*mean(walsyu.traits$lar1)+walsyu.coef[38,2]*mean(walsyu.traits$lmf)+
  walsyu.coef[39,2]*mean(walsyu.traits$smf)

sp.perform.trait.df=matrix(data=NA, nrow=24,ncol=2)
colnames(sp.perform.trait.df)=c("traits.only", "all.parameters")
row.names(sp.perform.trait.df)=c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
"dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
"lindme", "parach", "pittke", "pometo", "pseuin","pterme", "saprte", "syzyla", "walsyu")

sp.perform.trait.df[22,1]=saprte.perform.trait

# Growth performance of species considering all variables

pittke.coef[79,2]*mean(pittke.traits$lma)+
  pittke.coef[80,2]*mean(pittke.traits$rmf)+
  pittke.coef[81,2]*mean(pittke.traits$mean.thick)+
  pittke.coef[82,2]*mean(pittke.traits$ssl)+
  pittke.coef[83,2]*mean(pittke.traits$lar1)+
  pittke.coef[84,2]*mean(pittke.traits$lmf)+
  pittke.coef[85,2]*mean(pittke.traits$smf)+
  pittke.coef[87,2]*mean(pittke.traits$perc.canopy.open)+
  pittke.coef[88,2]*exp(mean(pittke.traits$Comp1))+
  pittke.coef[89,2]*exp(mean(pittke.traits$Comp2))+
  pittke.coef[90,2]*(mean(pittke.traits$lma)*mean(pittke.traits$smf))+
  pittke.coef[91,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf))+
  pittke.coef[92,2]*(mean(pittke.traits$lma)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[93,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[94,2]*(mean(pittke.traits$lma)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[95,2]*(mean(pittke.traits$lmf)*mean(pittke.traits$lar1))+
  pittke.coef[96,2]*(mean(pittke.traits$lmf)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[97,2]*(mean(pittke.traits$smf)*mean(pittke.traits$mean.thick))+
  pittke.coef[98,2]*(mean(pittke.traits$smf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[99,2]*(mean(pittke.traits$smf)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[100,2]*(mean(pittke.traits$rmf)*mean(pittke.traits$mean.thick))+
  pittke.coef[101,2]*(mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[102,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[103,2]*(mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[104,2]*(mean(pittke.traits$ssl)*mean(pittke.traits$mean.thick))+
  pittke.coef[105,2]*(mean(pittke.traits$ssl)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[106,2]*(mean(pittke.traits$mean.thick)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[107,2]*(mean(pittke.traits$mean.thick)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[108,2]*(mean(pittke.traits$lar1)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[109,2]*(mean(pittke.traits$lma)*mean(pittke.traits$smf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[110,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*mean(pittke.traits$perc.canopy.open))+
  pittke.coef[111,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[112,2]*(mean(pittke.traits$smf)*mean(pittke.traits$mean.thick)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[113,2]*(mean(pittke.traits$rmf)*mean(pittke.traits$mean.thick)*exp(mean(pittke.traits$Comp1)))+
  pittke.coef[114,2]*(mean(pittke.traits$lma)*mean(pittke.traits$rmf)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[115,2]*(mean(pittke.traits$lmf)*mean(pittke.traits$lar1)*exp(mean(pittke.traits$Comp2)))+
  pittke.coef[116,2]*(mean(pittke.traits$ssl)*mean(pittke.traits$mean.thick)*exp(mean(pittke.traits$Comp2)))





