library(picante)
library(rethinking)
library(rstan)
library(colordistance)
library(fields)

setwd("~/Documents/Intrasp.perform.land/intra.models/New.Results")

pseuin.mod=readRDS("pseuin.mod.RData")
parach.mod=readRDS("parach.mod.RData")
pittke.mod=readRDS("pittke.mod.RData")

#### Extract 1000 iterations from the posterior of the models into data frames ####

post.pseuin=extract.samples(pseuin.mod, n=1000)
post.parach=extract.samples(parach.mod, n=1000)
post.pittke=extract.samples(pittke.mod, n=1000)

#### Eliminate coefficients except for main effects and turn into data frame ####

post.pseuin.2=as.data.frame(post.pseuin[3:18])
post.parach.2=as.data.frame(post.parach[3:18])
post.pittke.2=as.data.frame(post.pittke[3:18])

# Make 1000 empty matrices where species are rows and parameters are columns combined into a list

matty=list()
for(i in 1:1000){
  matty[[i]]=matrix(data=NA, nrow=3, ncol=16)
  row.names(matty[[i]])=c("pseuin","parach", "pittke")
  colnames(matty[[i]])=c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13",
                         "b14", "b15", "b16")
}

# Select matrix j of 1000 and turn matrix into data frame
# Select species data frame i of 3
# Put row j of species i data frame into row i of matrix/dataframe
# Loop through each of the 3 species in listy, taking first row of each and putting into matrix 1
# Then loop through 3 species again until last row (1000) of each is placed into matrix 10000
# Output the now full matrices back into list matty

listy=list(post.pseuin.2, post.parach.2, post.pittke.2)

for(j in 1:1000){
  for(i in 1:3){
    new.mat=matty[[j]]
    new.mat.2=as.data.frame(new.mat)
    dat=listy[[i]]
    new.mat.2[i,]=dat[j,]
    matty[[j]]=new.mat.2
  }
}

### Save filled data frames so don't have to resample every time ####

save(matty, file="sample.coeff.df.Rdata") # Includes all species from original analysis

### Calculate the euclidean distance for each matrix ####
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

### Determine the mean for the distances of the 1000 matrices (mean of means) ####

mean.data=apply(simplify2array(dist.mat), 1, mean)

### Get the means back into a dist object ####

mean.mat=matrix(0, nrow=2, ncol=2)

colnames(mean.mat)=c("pseuin", "parach")
row.names(mean.mat)=c("parach" ,"pittke")

new.triag=which(lower.tri(mean.mat, diag=TRUE), arr.ind=TRUE)
mean.mat[new.triag]=mean.data

# write out the mean matrix and add pseuin as row 1 and pittke as column 3 with 0 so
# can be read back in and be converted to distance matrix correctly

write.csv(mean.mat, file="mean.mat.csv")

# Convert mean matrix to dist object to build dendrogram
# Rename the dist object attributes Labels to remove the "extra"

mean.mat=read.csv("mean.mat.csv", header=T, row.names = 1)
mean.dist=as.dist(mean.mat)

### Generate dendrograms ####

dendro=hclust(mean.dist, method="average")

plot(dendro)
plot(mean.dist)

### Plotting ####

mean.dist.mat=as.matrix(mean.dist)
row.names(mean.dist.mat)=c("P. indochinensis","P. chinensis","P. kerrii")
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

new.function(mean.dist.mat) # didn't use this plot

par(mar=c(5.1,4.1,4.1,2.1))
dim=ncol(mean.dist.mat)
image.plot(1:dim, 1:dim, mean.dist.mat,axes = FALSE, xlab="", ylab="", col = hcl.colors(12, "Terrain",rev = TRUE), legend.lab="Distance")
axis(1, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=2)
axis(2, 1:dim, colnames(mean.dist.mat), cex.axis = 0.8, las=1)
text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", mean.dist.mat), cex=0.7, col="black")

########## significance difference between each model variable relationship with RGR for species ########

# read in the 10000 resampled parameter values
# convert them to matrix then distance objects

matty=readRDS("sample.coeff.df.RData")

# make separate data frames for each model variable (1000 iterations)
# Code below repeated for each model variable

lma.rmf.pc2.df=matrix(data=NA, nrow=3, ncol=1000)
rownames(lma.rmf.pc2.df)=rownames(matty[[1]])
for(i in 1:1000){
  lma.rmf.pc2.df[,i]=(matty[[i]])$b16
}

## two.sided t.test between all pairs of species

sp.mean.par.t.test.results=matrix(data=NA, nrow=3, ncol=3)
colnames(sp.mean.par.t.test.results)=rownames(matty[[1]])
rownames(sp.mean.par.t.test.results)=rownames(matty[[1]])

for(j in 1:3){
  for(i in 1:3){
    t.output=t.test(lma.pc1.df[j,1:1000], lma.pc1.df[i,1:1000], alternative = "two.sided")
    sp.mean.par.t.test.results[i,j]=t.output$p.value
  }
}

write.csv(sp.mean.par.t.test.results, file="sp.mean.lma.rmf.pc2.t.test.results.csv")

# Output is the the p.value of the t.test
# Number of species that a species is significantly different from (p < 0.05), tallied up (t.test.totals.par.40.xlsx)
# Percent variables for a species that had a signicanlty different relationship
# from the other 3 species was determined. For example: PSEUIN has a significantly
# different relationship with growth from the other 6 species for 20/15 of it's variables (54%)
# Information presented in Table 1 of manuscript



  
