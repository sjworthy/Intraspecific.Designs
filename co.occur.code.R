setwd("~/Documents/Intrasp.perform.land/Data")
library(ggplot2)
library(cooccur)
library(vegan)

abund.mat=read.csv("occur.mat.abund.40.csv", header=T, row.names=1)
site.species.mat.pa=decostand(abund.mat, method="pa")
row.names(site.species.mat.pa)=c("P. chinensis", "P. kerrii", "P. indochinensis")

#### Determining Co-Occurrence Patterns ####
output=cooccur(site.species.mat.pa, type = "spp_site", spp_names = TRUE)
summary(output)
# 0 Positive
# 3 Negative
# 0 Random

plot(output) # plot isn't usable

obs.v.exp(output)

