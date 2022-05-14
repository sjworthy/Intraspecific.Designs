# Stan and Rstan code repeated for each of the seven species

setwd("~/Documents/GitHub/Intraspecific.Designs")

library(rstan)
library(rethinking)

readRDS("pseuin.mod.RData")
final.data=read.csv("final.data.3.csv", header=T, row.names=1)

pseuin=final.data[final.data$sp=="PSEUIN",]
output.thick=scale(log(pseuin$mean.thick))
output.ssl=scale(log(pseuin$ssl))
output.lar1=scale(log(pseuin$lar1))
output.lmf=scale(log(pseuin$lmf))
output.smf=scale(log(pseuin$smf))
output.rmf=scale(log(pseuin$rmf))
output.lma=scale(log(pseuin$lma))
output.new.rgr.h=scale(log(pseuin$new.rgr.h))
output.per.canopy.open=scale(log(pseuin$perc.canopy.open))
output.ini_size=scale(log(pseuin$size))
pseuin$log_thick=output.thick
pseuin$log_ssl=output.ssl
pseuin$log_lar=output.lar1
pseuin$log_lmf=output.lmf
pseuin$log_smf=output.smf
pseuin$log_rmf=output.rmf
pseuin$log_lma=output.lma
pseuin$log_rgr=output.new.rgr.h
pseuin$log_light=output.per.canopy.open
pseuin$log_ini_size=output.ini_size
write.csv(pseuin, file="pseuin.csv")

pseuin = read.csv("PSEUIN.data.csv", header=T, row.names=1) 
pseuin.2<-pseuin[,c(15,16,25:28,30,31)]

parach = read.csv("PARACH.data.csv", header=T, row.names = 1)
parach.2<-parach[,c(15,16,25:30)]

pittke = read.csv("PITTKE.data.csv", header=T, row.names = 1)
pittke.2<-pittke[,c(15,16,25:30)]


mod <-map2stan(
alist(
	# likelihood
	log_rgr ~ dnorm(mu, sigma),
	
	#linear models
    mu <- a_plot[plot_id] + b1*(log_lma) + b2*(log_rmf)
    + b3*(log_ini_size) + b4*(log_light) + b5*(Comp1) + b6*(Comp2)
    + b7*(log_lma)*(log_rmf) + b8*(log_lma)*(log_light)
    + b9*(log_lma)*(Comp1) + b10*(log_lma)*(Comp2) + b11*(log_rmf)*(log_light)
    + b12*(log_rmf)*(Comp1) + b13*(log_rmf)*(Comp2) + b14*(log_lma)*(log_rmf)*(log_light) 
	  + b15*(log_lma)*(log_rmf)*(Comp1)+ b16*(log_lma)*(log_rmf)*(Comp2),
       
    a_plot[plot_id] ~ dnorm(z, sigma_plot),
	  z ~ dnorm(0,1),
    
    c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,  b12,  b13,  b14,  b15, b16) ~ dnorm(0,10),
	  sigma_plot ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2)
    ),
    
    data=pittke.2, warmup=5000, iter=50000, chains=4, cores=2)
    
    
    saveRDS(mod, "pittke.mod.RData")
    plot(mod) # Trace Plot
    precis(mod, depth=2, prob=0.95)
    as.numeric(coef(mod, [1:23]))
    plot(precis(mod, depth=2))
    plot(precis(mod, depth=2, pars="b"))
    stancode(mod)
    
saveRDS(mod, "mod.RData") # save entire model object
mod=readRDS("mod.RData") # read in model object

coeff=as.data.frame(mod@coef) # gives you the coefficients of the models in dataframe
write.csv(coeff, "pseuin.coeff.output.csv")

all.output=precis(mod, depth=2, prob=0.95) # gives you the mean, sd, CI, n_eff, Rhat
all.output.2=as.data.frame(all.output) # extracts and converts to data frame for output
write.csv(all.output.2, "pittke.output.csv")


