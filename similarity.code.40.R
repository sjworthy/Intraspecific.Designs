library(picante)
library(rethinking)
library(rstan)
library(colordistance)
library(fields)

setwd("~/Desktop/Intrasp.perform.land/intra.models/Manuscript.Data")

pseuin.mod=readRDS("pseuin.output.RData")
amoodu.mod=readRDS("amoodu.output.RData")
dichge.mod=readRDS("dichge.output.RData")
parach.mod=readRDS("parach.output.RData")
pittke.mod=readRDS("pittke.output.RData")
saprte.mod=readRDS("saprte.output.RData")
walsyu.mod=readRDS("walsyu.mod.RData")

### Write out coefficient tables for models with significant variables

pseuin.coeff=as.data.frame(precis(pseuin.mod, depth = 2, prob=0.95)@output)
write.csv(pseuin.coeff, "pseuin.coeff.table.csv")
amoodu.coeff=as.data.frame(precis(amoodu.mod, depth = 2, prob=0.95)@output)
write.csv(amoodu.coeff, "amoodu.coeff.table.csv")
dichge.coeff=as.data.frame(precis(dichge.mod, depth = 2, prob=0.95)@output)
write.csv(dichge.coeff, "dichge.coeff.table.csv")
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
post.amoodu=extract.samples(amoodu.mod, n=1000)
post.dichge=extract.samples(dichge.mod, n=1000)
post.parach=extract.samples(parach.mod, n=1000)
post.pittke=extract.samples(pittke.mod, n=1000)
post.saprte=extract.samples(saprte.mod, n=1000)
post.walsyu=extract.samples(walsyu.mod, n=1000)

### Eliminate coefficients except for main effects and turn into data frame

post.pseuin.2=as.data.frame(post.pseuin[3:40])
post.amoodu.2=as.data.frame(post.amoodu[3:40])
post.dichge.2=as.data.frame(post.dichge[3:40])
post.parach.2=as.data.frame(post.parach[3:40])
post.pittke.2=as.data.frame(post.pittke[3:40])
post.saprte.2=as.data.frame(post.saprte[3:40])
post.walsyu.2=as.data.frame(post.walsyu[3:40])

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

### Eliminate all species with < 40 individuals

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

### Determine the mean for the distances of the 1000 matrices (mean of means)

mean.data=apply(simplify2array(dist.mat), 1, mean)

### Get the means back into a dist object 

mean.mat=matrix(0, nrow=6, ncol=6)

colnames(mean.mat)=c("pseuin", "amoodu" ,"dichge", "parach" ,"pittke" ,"saprte")
row.names(mean.mat)=c("amoodu" ,"dichge", "parach" ,"pittke" ,"saprte", "walsyu")

new.triag=which(lower.tri(mean.mat, diag=TRUE), arr.ind=TRUE)
mean.mat[new.triag]=mean.data

# write out the mean matrix and add pseuin as row 1 and walsyu as column 7 with 0 so
# can be read back in and be converted to distance matrix correctly

write.csv(mean.mat, file="mean.mat.40.csv")

# Convert mean matrix to dist object to build dendrogram
# Rename the dist object attributes Labels to remove the "extra"

mean.mat=read.csv("mean.mat.40.csv", header=T, row.names = 1)
mean.dist=as.dist(mean.mat)

### Generate dendrograms

dendro=hclust(mean.dist, method="average")

plot(dendro)
plot.hclust(dendro)
plot(mean.dist)

### Plotting

mean.dist.mat=as.matrix(mean.dist)
row.names(mean.dist.mat)=c("P. indochinensis", "A. duodecimantha",
                           "D. gelonioides", "P. chinensis",
                           "P. kerrii","S. ternata", "W. yunnanensis")
colnames(mean.dist.mat)=row.names(mean.dist.mat)

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

par(mar=c(8,10,1,1))
dim=ncol(mean.dist.mat)
image.plot(1:dim, 1:dim, mean.dist.mat,axes = FALSE, xlab="", ylab="", col = hcl.colors(12, "Terrain",rev = TRUE), legend.lab="Distance")
axis(1, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=2)
axis(2, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", mean.dist.mat), cex=0.7, col="black")

########## significance difference between each model variable relationship with RGR for species ########

# read in the 10000 resampled parameter values
# convert them to matrix then distance objects

matty=readRDS("sample.coeff.df.40.Rdata")

# make separate data frames for each model variable (1000 iterations)
# Code below repeated for each model variable

LMA.Light.df=matrix(data=NA, nrow=7, ncol=1000)
rownames(LMA.Light.df)=rownames(matty[[1]])
for(i in 1:1000){
  LMA.Light.df[,i]=(matty[[i]])$b14
}

## two.sided t.test between all pairs of species

sp.mean.par.t.test.results=matrix(data=NA, nrow=7, ncol=7)
colnames(sp.mean.par.t.test.results)=rownames(matty[[1]])
rownames(sp.mean.par.t.test.results)=rownames(matty[[1]])

for(j in 1:7){
  for(i in 1:7){
    t.output=t.test(LMA.df[j,1:1000], LMA.Light.df[i,1:1000], alternative = "two.sided")
    sp.mean.par.t.test.results[i,j]=t.output$p.value
  }
}

write.csv(sp.mean.par.t.test.results, file="40.sp.mean.LMA.Light.t.test.results.csv")

# Output is the the p.value of the t.test
# Number of species that a species is significantly different from (p < 0.05), tallied up (t.test.totals.par.40.xlsx)
# Percent variables for a species that had a signicanlty different relationship
# from the other 6 species was determined. For example: DICHGE has a significantly
# different relationship with growth from the other 6 species for 20/37 of it's variables (54%)
# Information presented in Table 1 of manuscript



  
