library(picante)
library(rethinking)
library(rstan)

setwd("~/Desktop/Intrasp.perform.land/intra.models")

### Read in the results of the model for each species

pseuin.mod=readRDS("pseuin.output.RData")
alphmo.mod=readRDS("alphmo.mod.100000.RData")
alsean.mod=readRDS("alsean.output.100000.RData")
alsepe.mod=readRDS("alsepe.output.100000.RData")
amoodu.mod=readRDS("amoodu.output.RData")
amooyu.mod=readRDS("amooyu.output.100000.RData")
baccra.mod=readRDS("baccra.mod.100000.RData")
cappfo.mod=readRDS("cappfo.output.100000.RData")
dichge.mod=readRDS("dichge.output.RData")
diosha.mod=readRDS("diosha.output.100000.RData")
diosxi.mod=readRDS("diosxi.output.100000.RData")
drypho.mod=readRDS("drypho.output.100000.RData")
garcco.mod=readRDS("garcco.output.100000.RData")
garufl.mod=readRDS("garufl.output.100000.RData")
harpcu.mod=readRDS("harpcu.output.100000.RData")
knemfu.mod=readRDS("knemfu.mod.100000.RData")
lindme.mod=readRDS("lindme.output.100000.RData")
parach.mod=readRDS("parach.output.RData")
pittke.mod=readRDS("pittke.output.RData")
pometo.mod=readRDS("pometo.output.100000.RData")
pterme.mod=readRDS("pterme.output.100000.RData")
saprte.mod=readRDS("saprte.output.RData")
syzyla.mod=readRDS("syzyla.mod.100000.RData")
walsyu.mod=readRDS("walsyu.mod.RData")

### Write out coefficient tables for models with significant variables

pseuin.coeff=as.data.frame(precis(pseuin.mod, depth = 2, prob=0.95)@output)
write.csv(pseuin.coeff, "pseuin.coeff.table.csv")
parach.coeff=as.data.frame(precis(parach.mod, depth = 2, prob=0.95)@output)
write.csv(parach.coeff, "parach.coeff.table.csv")
pittke.coeff=as.data.frame(precis(pittke.mod, depth = 2, prob=0.95)@output)
write.csv(pittke.coeff, "pittke.coeff.table.csv")
walsyu.coeff=as.data.frame(precis(walsyu.mod, depth = 2, prob=0.95)@output)
write.csv(walsyu.coeff, "walsyu.coeff.table.csv")
saprte.coeff=as.data.frame(precis(saprte.mod, depth = 2, prob=0.95)@output)
write.csv(saprte.coeff, "saprte.coeff.table.csv")

### Extract 1000 iterations from the posterior of the models into data frames

post.pseuin=extract.samples(pseuin.mod, n=1000)
post.alphmo=extract.samples(alphmo.mod, n=1000)
post.alsean=extract.samples(alsean.mod, n=1000)
post.alsepe=extract.samples(alsepe.mod, n=1000)
post.amoodu=extract.samples(amoodu.mod, n=1000)
post.amooyu=extract.samples(amooyu.mod, n=1000)
post.baccra=extract.samples(baccra.mod, n=1000)
post.cappfo=extract.samples(cappfo.mod, n=1000)
post.dichge=extract.samples(dichge.mod, n=1000)
post.diosha=extract.samples(diosha.mod, n=1000)
post.diosxi=extract.samples(diosxi.mod, n=1000)
post.drypho=extract.samples(drypho.mod, n=1000)
post.garcco=extract.samples(garcco.mod, n=1000)
post.garufl=extract.samples(garufl.mod, n=1000)
post.harpcu=extract.samples(harpcu.mod, n=1000)
post.knemfu=extract.samples(knemfu.mod, n=1000)
post.lindme=extract.samples(lindme.mod, n=1000)
post.parach=extract.samples(parach.mod, n=1000)
post.pittke=extract.samples(pittke.mod, n=1000)
post.pometo=extract.samples(pometo.mod, n=1000)
post.pterme=extract.samples(pterme.mod, n=1000)
post.saprte=extract.samples(saprte.mod, n=1000)
post.syzyla=extract.samples(syzyla.mod, n=1000)
post.walsyu=extract.samples(walsyu.mod, n=1000)

### Eliminate coefficients except for main effects and turn into data frame

post.pseuin.2=as.data.frame(post.pseuin[3:40])
post.alphmo.2=as.data.frame(post.alphmo[3:40])
post.alsean.2=as.data.frame(post.alsean[3:40])
post.alsepe.2=as.data.frame(post.alsepe[3:40])
post.amoodu.2=as.data.frame(post.amoodu[3:40])
post.amooyu.2=as.data.frame(post.amooyu[3:40])
post.baccra.2=as.data.frame(post.baccra[3:40])
post.cappfo.2=as.data.frame(post.cappfo[3:40])
post.dichge.2=as.data.frame(post.dichge[3:40])
post.diosha.2=as.data.frame(post.diosha[3:40])
post.diosxi.2=as.data.frame(post.diosxi[3:40])
post.drypho.2=as.data.frame(post.drypho[3:40])
post.garcco.2=as.data.frame(post.garcco[3:40])
post.garufl.2=as.data.frame(post.garufl[3:40])
post.harpcu.2=as.data.frame(post.harpcu[3:40])
post.knemfu.2=as.data.frame(post.knemfu[3:40])
post.lindme.2=as.data.frame(post.lindme[3:40])
post.parach.2=as.data.frame(post.parach[3:40])
post.pittke.2=as.data.frame(post.pittke[3:40])
post.pometo.2=as.data.frame(post.pometo[3:40])
post.pterme.2=as.data.frame(post.pterme[3:40])
post.saprte.2=as.data.frame(post.saprte[3:40])
post.syzyla.2=as.data.frame(post.syzyla[3:40])
post.walsyu.2=as.data.frame(post.walsyu[3:40])


### Make a list of the species data frames

listy=list(post.pseuin.2,post.alphmo.2,post.alsean.2,post.alsepe.2,post.amoodu.2,post.amooyu.2,
           post.baccra.2,post.cappfo.2,post.dichge.2,post.diosha.2,post.diosxi.2,post.drypho.2,post.garcco.2,
           post.garufl.2,post.harpcu.2,post.knemfu.2,post.lindme.2,post.parach.2,post.pittke.2,post.pometo.2,
           post.pterme.2,post.saprte.2,post.syzyla.2,post.walsyu.2)

# Make 1000 empty matrices where species are rows and parameters are columns combined into a list

matty=list()
for(i in 1:1000){
  matty[[i]]=matrix(data=NA, nrow=24, ncol=38)
  row.names(matty[[i]])=c("pseuin", "alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
                   "dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
                   "lindme", "parach", "pittke", "pometo", "pterme", "saprte", "syzyla", "walsyu")
  colnames(matty[[i]])=c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13",
                  "b14", "b15", "b16", "b17", "b18", "b19", "b20", "b21", "b22", "b23", "b24",
                  "b25", "b26", "b27", "b28", "b29", "b30", "b31", "b32", "b33", "b34", "b35",
                  "b36", "b37", "b38")
}

# Select matrix j of 1000 and turn matrix into data frame
# Select species data frame i of 24
# Put row j of species i data frame into row i of matrix/dataframe
# Loop through each of the 24 species in listy, taking first row of each and putting into matrix 1
# Then loop through 24 species again until last row (1000) of each is placed into matrix 10000
# Output the now full matrices back into list matty

for(j in 1:1000){
  for(i in 1:24){
    new.mat=matty[[j]]
    new.mat.2=as.data.frame(new.mat)
    dat=listy[[i]]
    new.mat.2[i,]=dat[j,]
    matty[[j]]=new.mat.2
  }
}

### Save filled data frames so don't have to resample every time

save(matty, file="sample.coeff.df.Rdata") # Includes all species from original analysis

### Eliminate all species with >= 40 individuals

for(i in 1:1000){
  new.mat=matty[[i]]
  new.mat.2=new.mat[c(1,5,9,18,19,22,24),]
  matty[[i]]=new.mat.2
}

### Save filled data frames so don't have to resample every time for species with > 40 individuals

save(matty, file="sample.coeff.df.40.Rdata") # includes only 7 species in manuscript (with at least 40 individuals)

### Calculate the euclidean distance for each matrix 
# Turn data frames in matty back into matrices
# calculate euclidean distance for each of the 1000 matrices

matty.2=list()
for(i in 1:1000){
  matty.2[[i]]=as.matrix(matty[[i]])
}
  
dist.mat=list()
for(i in 1:1000){
  dist.mat[[i]]=dist(matty.2[[i]], method="euclidean")
}


test=unlist(dist.mat)

### Determine the average and sd for the distances of the 1000 matrices

mean.data=apply(simplify2array(dist.mat), 1, mean)
sd.dat=apply(simplify2array(dist.mat), 1, sd)

### Get the means back into a dist object 

mean.mat=matrix(0, nrow=23, ncol=23)

### To get the names for the columns and rows

attr(dist.mat[[1]], "Labels")

colnames(mean.mat)= c("pseuin", "alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
"dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
"lindme", "parach", "pittke", "pometo", "pterme", "saprte", "szyla")
row.names(mean.mat)= c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
"dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
"lindme", "parach", "pittke", "pometo", "pterme", "saprte", "syzyla", "walsyu")

# For just species >= 40 individuals

mean.mat=matrix(0, nrow=6, ncol=6)

colnames(mean.mat)=c("pseuin", "amoodu" ,"dichge", "parach" ,"pittke" ,"saprte")
row.names(mean.mat)=c("amoodu" ,"dichge", "parach" ,"pittke" ,"saprte", "walsyu")

new.triag=which(lower.tri(mean.mat, diag=TRUE), arr.ind=TRUE)
mean.mat[new.triag]=mean.data

# write out the mean matrix and add pseuin as row 1 and walsyu as column 24 with 0 so
# can be read back in and be converted to distance matric correctly
write.csv(mean.mat, file="mean.mat.40.csv")

# Convert mean matrix to dist object to build dendrogram
# Rename the dist object attributes Labels to remove the "extra"

mean.mat=read.csv("mean.mat.csv", header=T, row.names = 1)

mean.mat=read.csv("mean.mat.40.csv", header=T, row.names = 1)

mean.dist=as.dist(mean.mat)

### Generate dendrograms

dendro=hclust(mean.dist, method="average")
dendro.ward=hclust(mean.dist, method = "complete")

plot(dendro)
plot(dendro.ward)
plot(dendro, hang=-1)
plot.hclust(dendro)
plot(mean.dist)

### Plotting

mean.dist.mat=as.matrix(mean.dist)
row.names(mean.dist.mat)=c("P. indochinensis", "A. duodecimantha",
                           "D. gelonioides", "P. chinensis",
                           "P. kerrii","S. ternata", "W. yunnanensis")
colnames(mean.dist.mat)=row.names(mean.dist.mat)

install.packages("colordistance")
library(colordistance)

# To force heatmapColorDistance function to using method="average" for hclust,
# alter the function portion Rowv = as.dendrogram(hclust(clust, method="average"))
# Also alter margins to 8,10

new.function=function (clusterList_or_matrixObject, main = NULL, col = "default", 
          margins = c(8, 10), ...) 
{
  obj <- clusterList_or_matrixObject
  if (is.list(obj)) {
    obj <- getColorDistanceMatrix(obj)
  }
  else if (!is.matrix(obj)) {
    stop("Argument is not a list (extractClusters or getHistList object)", 
         " or a distance matrix (getColorDistanceMatrix object)")
  }
  if (col[1] == "default") {
    col <- colorRampPalette(c("royalblue4", "ghostwhite", 
                              "violetred2"))(n = 299)
  }
  clust <- as.dist(obj)
  gplots::heatmap.2(obj, symm = TRUE, col = col, Rowv = as.dendrogram(hclust(clust, method="average")), 
                    main = main, trace = "none", density.info = "none", key.xlab = "Color distance score", 
                    key.title = NA, keysize = 1, revC = T, srtCol = 35, na.color = "grey", 
                    margins = margins, offsetRow = 0, offsetCol = 0, ...)
}

new.function(mean.dist.mat)

library(fields)

dim=ncol(mean.dist.mat)
image.plot(1:dim, 1:dim, mean.dist.mat,axes = FALSE, xlab="", ylab="", col = hcl.colors(12, "Mint",rev = TRUE), legend.lab="Distance")
axis(1, 1:dim, colnames(mean.dist.mat), cex.axis = 0.5, las=3)
axis(2, 1:dim, colnames(mean.dist.mat), cex.axis = 0.5, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", mean.dist.mat), cex=0.6)

## for >= 40

par(mar=c(8,10,1,1))
dim=ncol(mean.dist.mat)
image.plot(1:dim, 1:dim, mean.dist.mat,axes = FALSE, xlab="", ylab="", col = hcl.colors(12, "Terrain",rev = TRUE), legend.lab="Distance")
axis(1, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=2)
axis(2, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", mean.dist.mat), cex=0.7, col="black")


install.packages("cluster")
library(cluster)

test=agnes(mean.dist.mat, diss=TRUE, method = "average")

test.2=diana(mean.dist.mat, diss=TRUE, stand = FALSE)

library(gplots)
heatmap.2(mean.dist.mat)

## Calculate SES values for distance matrix

## Make the empty distance matrices

sim.matty=list()
for(i in 1:999){
  sim.matty[[i]]=matrix(data=NA, nrow=23, ncol=23)
  row.names(sim.matty[[i]])=c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
                          "dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
                          "lindme", "parach", "pittke", "pometo", "pterme", "saprte", "syzyla", "walsyu")
  colnames(sim.matty[[i]])=c("pseuin","alphmo","alsean","alsepe","amoodu","amooyu",
                         "baccra","cappfo","dichge","diosha","diosxi","drypho","garcco",
                         "garufl","harpcu","knemfu","lindme","parach","pittke","pometo",
                         "pterme","saprte","syzyla")
}

## for loop to sample the mean distance matrix and plug each sample 
## into 1 of 999 matrices from sim.matty
for(i in 1:999){
  output=sample(mean.dist)
  sim.matty[[i]][lower.tri(sim.matty[[i]], diag = TRUE)] <- output
}

## add pseuin as row 1 and walsyu as column 24 with 0 so
# can be read back in and converted to distance matrix correctly

## make new matrices of 1 row and 23 columns
new.mat=list()
for(i in 1:999){
  new.mat[[i]]=matrix(data=NA, nrow=1, ncol=23)
  row.names(new.mat[[i]])="pseuin"
}

# rbind new matrices to sampled ones to get 24 rows with blank row first
rbin.sim.matts=list()
for(i in 1:999){
rbin.sim.matts[[i]]=rbind(new.mat[[i]], sim.matty[[i]])
}

# cbind new matrices with column of 0 to get 24 rows and 24 columns
cbin.sim.matts=list()
for(i in 1:999){
  cbin.sim.matts[[i]]=cbind(rbin.sim.matts[[i]], 0)
  colnames(cbin.sim.matts[[i]])[24]="walsyu"
}

# replace all NA with 0 in matrix
final.sim.matts=list()
for(i in 1:999){
  cbin.sim.matts[[i]][is.na(cbin.sim.matts[[i]])]=0
  final.sim.matts[[i]]=cbin.sim.matts[[i]]
}
  
# Turn all of the matrices into dist objects

sim.dist.list=list()
for(i in 1:999){
  sim.dist.list[[i]]=as.dist(final.sim.matts[[i]])
}

### Determine the average and sd for the distances of the 1000 matrices

sim.null.mean=apply(simplify2array(sim.dist.list), 1, mean)
sim.null.sd=apply(simplify2array(sim.dist.list), 1, sd)

install.packages("otuSummary")

library(otuSummary)
# convert distance matrix to dataframe
mean.dist.df=matrixConvert(mean.dist)

# convert mean numbers into dist object to be converted into data frame
mean.matrix=matrix(data=NA, nrow=23, ncol=23)
mean.matrix[lower.tri(mean.matrix, diag = TRUE)] <- sim.null.mean
colnames(mean.matrix)=c("pseuin","alphmo","alsean","alsepe","amoodu","amooyu",
                        "baccra","cappfo","dichge","diosha","diosxi","drypho","garcco",
                        "garufl","harpcu","knemfu","lindme","parach","pittke","pometo",
                        "pterme","saprte","syzyla")
row.names(mean.matrix)=c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
                         "dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
                         "lindme", "parach", "pittke", "pometo", "pterme", "saprte", "syzyla", "walsyu")
mean.matrix=cbind(mean.matrix,0)
colnames(mean.matrix)[24]="walsyu"
null.new.mat=matrix(data=NA, nrow=1, ncol=24)
mean.matrix.2=rbind(null.new.mat, mean.matrix)
rownames(mean.matrix.2)[1]="pseuin"

sim.null.mean.df=matrixConvert(as.dist(mean.matrix.2))

# convert sd numbers into dist object to be converted into data frame
sd.matrix=matrix(data=NA, nrow=23, ncol=23)
sd.matrix[lower.tri(sd.matrix, diag = TRUE)] <- sim.null.sd
colnames(sd.matrix)=c("pseuin","alphmo","alsean","alsepe","amoodu","amooyu",
                        "baccra","cappfo","dichge","diosha","diosxi","drypho","garcco",
                        "garufl","harpcu","knemfu","lindme","parach","pittke","pometo",
                        "pterme","saprte","syzyla")
row.names(sd.matrix)=c("alphmo", "alsean", "alsepe", "amoodu", "amooyu", "baccra", "cappfo",
                         "dichge", "diosha", "diosxi", "drypho", "garcco", "garufl", "harpcu", "knemfu",
                         "lindme", "parach", "pittke", "pometo", "pterme", "saprte", "syzyla", "walsyu")
sd.matrix=cbind(sd.matrix,0)
colnames(sd.matrix)[24]="walsyu"
sd.null.new.mat=matrix(data=NA, nrow=1, ncol=24)
sd.matrix.2=rbind(sd.null.new.mat, sd.matrix)
rownames(sd.matrix.2)[1]="pseuin"

sim.null.sd.df=matrixConvert(as.dist(sd.matrix.2))

### calculate SES values
sim.ES=mean.dist.df$dist-sim.null.mean.df$dist
sim.SES=(mean.dist.df$dist-sim.null.mean.df$dist)/sim.null.sd.df$dist
p.vall.all=apply(cbind(mean.dist.df$dist,sim.null.mean.df$dist),MARGIN=1,rank)[1,]/1000

SES=observed-mean(null)/sd(null)

Sim.results=mean.dist.df
Sim.results[,4]=sim.null.mean.df$dist
colnames(Sim.results)[4]="mean.null"
Sim.results[,5]=sim.null.sd.df$dist
colnames(Sim.results)[5]="sd.null"
Sim.results[,6]=sim.ES
colnames(Sim.results)[6]="Effect.Size"
Sim.results[,7]=sim.SES
colnames(Sim.results)[7]="SES"
Sim.results[,8]=p.vall.all
colnames(Sim.results)[8]="p.value"


########## significance difference between each pair ########

# read in the 10000 resampled parameter values
# convert them to matrix then distance objects
matty=readRDS("sample.coeff.df.Rdata")

# convert them to dataframe for pairs
library(otuSummary)
convert.matty=list()
for(i in 1:1000){
  convert.matty[[i]]=matrixConvert(dist.mat[[i]])
}

## combine all the distances into a single data frame
all.data=convert.matty[[1]]
output=matrix(data=NA, nrow=276, ncol=999)
for(i in 2:1000){
output[,i-1]=convert.matty[[i]]$dist
}

# > 40
## combine all the distances into a single data frame
all.data=convert.matty[[1]]
output=matrix(data=NA, nrow=21, ncol=999)
for(i in 2:1000){
  output[,i-1]=convert.matty[[i]]$dist
}

all.data=cbind(all.data,output)

write.csv(all.data, "all.dist.df.40.csv")

## two.sided t.test between all pairs of species

t.test.results=convert.matty[[1]]
t.test.results=t.test.results[,1:2]
t.test.results[,3]=NA
colnames(t.test.results)[3]="row.1.p.value"

for(i in 276:276){  
  t.output=t.test(all.data[275,3:1000], all.data[i,3:1000], alternative = "two.sided")
t.test.results[i,277]=t.output$p.value
}

write.csv(t.test.results, file="t.test.results.csv")

########## significance difference between each species mean of all parameters ########

# read in the 10000 resampled parameter values
# convert them to matrix then distance objects

matty=readRDS("sample.coeff.df.Rdata")

# data frame of species with 1000 mean parameters values

sp.mean.par=matrix(data=NA, nrow=24, ncol=1000)
rownames(sp.mean.par)=rownames(matty[[1]])

for(i in 1:1000){
sp.mean.par[,i]=rowMeans(matty[[i]])
}

write.csv(sp.mean.par, "sp.mean.par.csv")

## > 40

sp.mean.par=matrix(data=NA, nrow=7, ncol=1000)
rownames(sp.mean.par)=rownames(matty[[1]])

for(i in 1:1000){
  sp.mean.par[,i]=rowMeans(matty[[i]])
}

write.csv(sp.mean.par, "sp.mean.par.csv")

# make separate data frames for each varialbe (1000 iterations)

LMA.df=matrix(data=NA, nrow=7, ncol=1000)
rownames(LMA.df)=rownames(matty[[1]])
for(i in 1:1000){
  LMA.df[,i]=(matty[[i]])$b14
}

sp.mean.par.t.test.results=matrix(data=NA, nrow=7, ncol=7)
colnames(sp.mean.par.t.test.results)=rownames(matty[[1]])
rownames(sp.mean.par.t.test.results)=rownames(matty[[1]])

for(j in 1:7){
  for(i in 1:7){
    t.output=t.test(LMA.df[j,1:1000], LMA.df[i,1:1000], alternative = "two.sided")
    sp.mean.par.t.test.results[i,j]=t.output$p.value
  }
}

## two.sided t.test between all pairs of species

sp.mean.par.t.test.results=matrix(data=NA, nrow=24, ncol=24)
colnames(sp.mean.par.t.test.results)=rownames(matty[[1]])
rownames(sp.mean.par.t.test.results)=rownames(matty[[1]])

for(j in 1:24){
for(i in 1:24){
  t.output=t.test(sp.mean.par[j,1:1000], sp.mean.par[i,1:1000], alternative = "two.sided")
  sp.mean.par.t.test.results[i,j]=t.output$p.value
  }
}

write.csv(sp.mean.par.t.test.results, file="sp.mean.par.t.test.results.csv")

## two.sided t.test between all pairs of species > 40

sp.mean.par.t.test.results=matrix(data=NA, nrow=7, ncol=7)
colnames(sp.mean.par.t.test.results)=rownames(matty[[1]])
rownames(sp.mean.par.t.test.results)=rownames(matty[[1]])

for(j in 1:7){
  for(i in 1:7){
    t.output=t.test(sp.mean.par[j,1:1000], sp.mean.par[i,1:1000], alternative = "two.sided")
    sp.mean.par.t.test.results[i,j]=t.output$p.value
  }
}



## correlate number of non.sign with abundance

cor.df=matrix(data=NA, nrow=24, ncol=2)
rownames(cor.df)=rownames(matty[[1]])
cor.df[,1]=c(3,9,3,6,5,5,6,4,5,3,2,2,7,13,5,2,5,2,4,8,3,5,9,2)
cor.df[,2]=c(373,18,10,15,65,35,13,35,42,34,33,16,18,10,10,20,37,194,108,29,31,54,11,80)

cor.df.2=cor.df[2:24,]
cor.test(cor.df[,1], cor.df[,2])
cor.test(cor.df.2[,1], cor.df.2[,2])
plot(cor.df.2[,1], cor.df.2[,2])
sort(cor.df)


# Correlating average number of non. signifanct values with abundance

t.test.total.pars=read.csv("t.test.totals.pars.csv", header=T ,row.names=1)
cor.test(t.test.total.pars$average, t.test.total.pars$abundance)

# remove < 40 species from sp.mean.par.t.test
setwd("~/Desktop/Intrasp.perform.land/intra.models/sp.mean.pars")

input=read.csv("sp.mean.LMA.Light.t.test.results.csv", row.names=1)
alter=input[c(1,5,9,18,19,22,24),c(1,5,9,18,19,22,24)]
write.csv(alter, file="40.sp.mean.Thick.t.test.results.csv")

# correlation average distance and abundance >40
abund=c(373,65,42,194,108,54,80)
aver.dist=c(18.42184139,18.75116859,20.09211138,18.54479952,18.50195117,18.74313893,19.18999387)

