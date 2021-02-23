setwd("~/Desktop/Intrasp.perform.land/Data")
library(ggplot2)
library(cooccur)
library(vegan)

abund.mat=read.csv("occur.mat.abund.40.csv", header=T, row.names=1)
site.species.mat.pa=decostand(abund.mat, method="pa")
row.names(site.species.mat.pa)=c("A. duodecimantha", "D. gelonioides",
                                 "P. chinensis", "P. kerrii", "P. indochinensis",
                                 "S. ternata", "W. yunnanensis")

### Species pairs expected to have less than 1 cooccurrences removed
output=cooccur(site.species.mat.pa, type = "spp_site", spp_names = TRUE)
summary(output)
plot(output)
write.csv(output$results, file="co.occur.reduced.output.csv")

### All species pairs considered
output.1=cooccur(site.species.mat.pa, type = "spp_site", thresh = FALSE,
                 spp_names = TRUE)
summary(output.1)
cc.plot=plot(output.1, plotrand=TRUE)
cc.plot + theme(legend.position = "right")

write.csv(output.1$results, file="co.occur.all.output..40.csv")
