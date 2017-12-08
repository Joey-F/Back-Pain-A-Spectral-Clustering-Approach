
#####
#####
##### Install and Load the necessary packages #####

# install.packages(c(
#     "ExPosition",
#     "Matrix",
#     "irlba",
#     "RSpectra",
#     "mice",
#     "VIM",
#     "MASS",
#     "GGally",
#     "ggplot2",
#     "sfsmisc",
#     "rgl"
# ))

library(ExPosition)
library(Matrix)
library(irlba)
library(RSpectra)
library(mice)
library(VIM)
library(MASS)
library(GGally)
library(ggplot2)
library(sfsmisc)
library(rgl)


#####
#####
#####
##### Load and Adjust Variables #####

set.seed(23)

originalP <- read.csv(file="C:\\Users\\Joey\\Documents\\Math 285 Clustering\\Project\\Project Data.csv", 
    header = TRUE)
Project <- originalP

originalP <- read.csv(file="C:\\Users\\jfitc\\Google Drive\\Back Pain Clustering Project\\Data\\Project Data.csv", 
                      header = TRUE)
Project <- originalP


Project[,86] <- Project[,86]-1  # 1,2 Binary
Project[,24] <- Project[,24]-1  # 1,2 Binary

Project <- Project[,-(1:10)] # first 10 columns not used for clustering, only validation


#####
#####
#####
##########      Pre-Processing
{
##### Data Exploration ##### 
# 
# colMeans(is.na(originalP)) # get proportion missing for each variable
# 
# plot(sort(colMeans(is.na(originalP))))
# 
# 
# rowMeans(is.na(Project)) # get proportion missing for each observation
# 
# length(which(rowMeans(is.na(Project))>.4))
# 
#     #  8 rows have >40% of their data NA
#     # 14 rows have >25% of their data NA
#     # 78 rows have >10% of their data NA
# 
# plot(sort(rowMeans(is.na(Project))))
# 
##### 
##### 
##### 
##### Data Formatting & Subsetting #####


# identify continuous vs categorical


# Categorical index:
factor.ind <- c(122,14,98,99,102,101,106,105,104,103)-10 # first 10 columns removed
# 122 is trichotomous

# Continuous:
    # 12,17,121,38,28,25,85,4,10,7,21
    
# Ordinal:
    # 16,18,13,20,19,62,71,72,73,74,75,63,64,65,66,67,68,69,70,76,84,77,78,79,80,81,82,83,23,22
    
# Numerical index (ordinal + continuous)
numerical.ind <- c(12,17,121,38,28,25,85,21,
                    16,18,13,20,19,62,71,72,73,74,75,
                    63,64,65,66,67,68,69,70,76,84,77,
                    78,79,80,81,82,83,23,22)-10 # first 10 columns removed

bin.categorical.ind <- c(11,15,37,86,87,88,92,93,94,95,96,97,100)-10

bin.presence.ind <- c(24,26,27,29,30,31,32,33,34,35,36,39,40,41,42,43,44,
                    45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,
                    89,90,91,107 ,108 ,109 ,110,111,112,113 ,114 ,115,116,
                    117,118,119,120)-10 # first 10 columns removed

# 64 columns are binary


Project[,factor.ind] <- lapply(Project[,factor.ind], function(x) as.factor(x))

P.factors <- lapply(Project[,factor.ind], function(x) as.factor(x))
P.factors <- as.data.frame(P.factors)

P.numerical <- Project[,numerical.ind]

P.bin.cat <- Project[,bin.categorical.ind]

P.bin.pres <- Project[,bin.presence.ind]


#####
##### 
##### 
##### Deal with the Categorical Stuff #####

library(ExPosition)

# Remove the bad columns

bad <- which(colnames(P.factors) == "musclegroup_palp") # 50% missing

P.factors <- P.factors[,-bad]

P.factors <- makeNominalData(P.factors)

P.factors <- as.data.frame(P.factors)

# All NA's for categorical data have been imputed with the column mean. 
# Be careful with similarity calculation.

 
# start_risk: 1,2,3
# barb0: 4, 5, 6, 7, ,8 9, 10, 11
# pain_dis: 12, 13, 14, 15
# romext: 16, 17, 18
# romflex: 19, 20, 21
# romrotl: 22, 23, 24
# romrotr: 25, 26, 27
# romsidegll: 28, 29, 30
# romsideglr: 31, 32, 33

P.factors[,1:3] <- P.factors[,1:3]/sqrt(3)

P.factors[,4:11] <- P.factors[,4:11]/sqrt(8)

P.factors[,12:15] <- P.factors[,12:15]/sqrt(4)

P.factors[,16:18] <- P.factors[,16:18]/sqrt(3)

P.factors[,19:21] <- P.factors[,19:21]/sqrt(3)

P.factors[,22:24] <- P.factors[,22:24]/sqrt(3)

P.factors[,25:27] <- P.factors[,25:27]/sqrt(3)

P.factors[,28:30] <- P.factors[,28:30]/sqrt(3)

P.factors[,31:33] <- P.factors[,31:33]/sqrt(3)




#####
#####
#####
##### Deal with the Numerical (Continuous/Ordinal) Stuff #####

### Remap every column to a (0,1) interval:

scalednumerical <- apply(P.numerical, 2,
    function(x) (
        (x-min(x[!is.na(x)])) / (max(x[!is.na(x)])-min(x[!is.na(x)]))
        ))

scalednumerical <- as.data.frame(scalednumerical)


# Impute NA's with the column mean:

for (column in 1:ncol(scalednumerical)) {
    temp <- scalednumerical[,column]
    temp[is.na(temp)] <- mean(temp, na.rm=TRUE)
    scalednumerical[,column] <- temp
}

# All NA's for numerical data have been imputed with the column mean. 
# Be careful with similarity calculation.


#####
#####
#####
##### Deal with the "Categorical" Binary Stuff ##### 

# "Categorical" binary data: qualitative difference between 0, 1, NA

### Disjunctive coding

P.bin.cat <- makeNominalData(P.bin.cat)

P.bin.cat <- as.data.frame(P.bin.cat)


# All NA's for binary-categorical data have been imputed with the column mean. 
# Be careful with similarity calculation.


# P.bin.cat <- P.bin.cat/sqrt(2)


### Treat numerically, impute the mean

# for (column in 1:ncol(P.bin.cat)) {
#     temp <- P.bin.cat[,column]
#     temp[is.na(temp)] <- mean(temp, na.rm=TRUE)
#     P.bin.cat[,column] <- temp
# }


##### 
##### 
##### 
##### Deal with the "Presence/Absence" Binary Stuff #####


### "Presence/Absence" binary data: 0's are not qualitatively unique, but 1's are


for (column in 1:ncol(P.bin.pres)) {
    temp <- P.bin.pres[,column]
    temp[is.na(temp)] <- mean(temp, na.rm=TRUE)
    P.bin.pres[,column] <- temp
}


#####
##### 
##### 
##### Put it all together #####

Project <- cbind(scalednumerical, P.factors, P.bin.cat, P.bin.pres) # 38 + 33 + 26 + 51 = 148 columns

### *DING* Your data is now "numeric"

# Categorical: disjunctive coding, with NA -> 0

# Continuous: rescaled to (0,1) interval

# Binary Categorical: factor coding as 1/0/-1 for 1/NA/0

# Binary Presence/Absence: NA's set to 0


#####
##### 
##### 
}
#####
#####
##########      Explore the first 10 withheld variables
{
    
dataw <- originalP[,2:10]


#####
#####
##### Explore the NA patterns #####

# hist(sort(rowMeans(is.na(dataw)))) # get proportion missing for each observation

# barplot(colMeans(is.na(dataw))) # get proportion missing for each variable

# cor(is.na(dataw))

    # 1st, 2nd, 3rd variables are near-perfect correlated in terms of NAs

        # = status 12 months after

    # 4th, 5th, 6th variables are near-perfect correlated in terms of NAs

        # = status 3 months after


    ###
    ### Correlation between columns
    ###

# cor(dataw, use="pairwise.complete.obs")



#####
#####
##### Impute NAs #####

library(mice)

# md.pattern(dataw)   # see patterns of NA's within the data

# mice(dataw)   # summarize the NA stuff
    
    ###
    ### Remove any data points which are missing all data from 
    ###     any single period (2w, 3m, 12m)
    ###

bad13 <- which(is.na(dataw[,1]) & is.na(dataw[,2]) & is.na(dataw[,3])) # length = 237

bad46 <- which(is.na(dataw[,4]) & is.na(dataw[,5]) & is.na(dataw[,6])) # length = 195

bad79 <- which(is.na(dataw[,7]) & is.na(dataw[,8]) & is.na(dataw[,9])) # length = 155

badrows <- c(bad13,bad46,bad79)

data.comp <- dataw[-badrows,]

data.comp <- scale(data.comp)

data13 <- data.comp[,1:3]
data46 <- data.comp[,4:6]
data79 <- data.comp[,7:9]


# KNN imputation

library(VIM)

data13 <- kNN(data13, k=3)[,1:3]
data46 <- kNN(data46, k=3)[,1:3]
data79 <- kNN(data79, k=3)[,1:3]

data.imp <- cbind(data13, data46, data79)



#####
#####
##### Correlation Similarity Matrix #####

corrsim <- function(data.imp) {
    P.centered <- scale(data.imp, center=TRUE, scale=FALSE)
    #P.centered <- data.imp-colMeans(data.imp, na.rm=TRUE)
    P.centered <- data.matrix(P.centered)
    rowvar <- rowSums(P.centered^2) # store the variances for each row
    P.correlation <- P.centered/sqrt(rowvar)
    
    P.correlation <- tcrossprod(P.correlation) # calculate the correlation matrix (dot product all rows)
    
    diag(P.correlation) <- rep(0,nrow(P.correlation))
    P.correlation <- (P.correlation - min(P.correlation))/(max(P.correlation) - min(P.correlation))
    diag(P.correlation) <- rep(0, nrow(P.correlation))
    Similarity.V <- P.correlation
    return(Similarity.V)
}

Similarity.V <- corrsim(data.imp)


#####
#####
##### Diffusion Map Spectral Clustering #####

library(Matrix)
library(RSpectra)

k <- 4

dim <- k+1   # do you want to stop at the k or (k+1) eigenvector?

t = .5 # "fuzziness" parameter

Weights <- Similarity.V 
n <- nrow(Weights)
dvec_inv = 1/sqrt(rowSums(Weights^2)) # L2 length of each row

# normalize the correlation matrix by the row lengths
W_tilde = Diagonal(n,dvec_inv) %*% Weights %*% Diagonal(n,dvec_inv) 

EV <- eigs_sym(W_tilde, dim, 'LM')  # first k+1 eigenpairs
V <- EV$vectors     
lambda <- EV$value

# the first eigenvector is the noise vector; 2+ describe the clusters
V <- V[,2:dim]

# normalize V by the L2 row length
V_inv = 1/sqrt((rowSums(V^2)))
V <- matrix(rep(V_inv,dim-1), ncol=dim-1) * V

U <-  abs(matrix(rep(lambda[2:dim], each=n), ncol=dim-1))^(t)* V

validation.DiffMap <- kmeans(U, centers=k, nstart = 100)

validation.DiffMap <- validation.DiffMap$clus




#####
#####
##### Parallel Coordinates Plot #####

clusters.v <- validation.DiffMap

# table(clusters.v)

# Find the mean for each cluster

mean1 <- colMeans(data.imp[clusters.v==1,])
mean2 <- colMeans(data.imp[clusters.v==2,])
mean3 <- colMeans(data.imp[clusters.v==3,])
mean4 <- colMeans(data.imp[clusters.v==4,])


# Form a matrix with the cluster means

groupmeans <- rbind(mean1, mean2, mean3, mean4)
cluster <- as.factor(c(1:4))
groupmeans <- data.frame(groupmeans,cluster )
library(MASS)
library(GGally)
library(ggplot2)


# Plot with standardized axes:
ggparcoord(data = groupmeans, columns = 1:9, scale = "uniminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


# Plot with NON-standardized axes:
ggparcoord(data = groupmeans, columns = 1:9, scale = "std", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


#####
#####
##### Box Plots #####

# One plot for each variable; each plot contains K many boxplots
    
    

# clusters.v <- validation.DiffMap
# 
# windows(20,20)
# par(mfrow=c(3,3))
# boxplot(gen12m~clusters.v, data=data.imp, main = "gen12m")
# boxplot(vasl12m~clusters.v, data=data.imp, main = "vasl12m")
# boxplot(rmprop12m~clusters.v, data=data.imp, main = "rmprop12m")
# boxplot(gen3m~clusters.v, data=data.imp, main = "gen3m")
# boxplot(vasl3m~clusters.v, data=data.imp, main = "vasl3m")
# boxplot(rmprop3m~clusters.v, data=data.imp, main = "rmprop3m")
# boxplot(gen2w~clusters.v, data=data.imp, main = "gen2w")
# boxplot(vasl2w~clusters.v, data=data.imp, main = "vasl2w")
# boxplot(rmprop2w~clusters.v, data=data.imp, main = "rmprop2w")
# 
# par(mfrow=c(1,1))


# One boxplot for each cluster; each plot contains P many boxplots

library("sfsmisc")

# boxplot.matrix(as.matrix(data.imp[clusters.v==1,]), use.cols=TRUE, ylim=c(-3,3))
# boxplot.matrix(as.matrix(data.imp[clusters.v==2,]), use.cols=TRUE, ylim=c(-3,3))
# boxplot.matrix(as.matrix(data.imp[clusters.v==3,]), use.cols=TRUE, ylim=c(-3,3))
# boxplot.matrix(as.matrix(data.imp[clusters.v==4,]), use.cols=TRUE, ylim=c(-3,3))

# can specify add=T to overlay a boxplot over a previous boxplot



}
#####
#####
##### 
##########      Spectral Clustering on the Data Sub-Rows
{
#####
#####
#####
##### Processing - ONLY RUN ONCE #####

# Only analyze the rows from the validation data, so we can 
#   compare our results

library(irlba)
library(RSpectra)

Project.full <- Project
Project <- Project[-badrows,]
dim(Project)

# Should be dimension 618 148


##### 
#####
#####
##### Correlation Similarity Measure #####


Similarity <- corrsim(Project)


##### 
##### 
##### 
##### Clustering #####

k <- 4

t = .5  # "fuzziness" parameter

Weights <- Similarity 
n <- nrow(Weights)

# # Row/Column symmetric weighting Process
# dvec_inv = 1/sqrt(rowSums(Weights))
# # normalize the correlation matrix by the row lengths
# W_tilde = Diagonal(n,dvec_inv) %*% Weights %*% Diagonal(n,dvec_inv)


### Probability distribution L1 weighting

dvec_inv = 1/rowSums(Weights)
# normalize the correlation matrix by the row lengths
W_tilde = diag(dvec_inv) %*% Weights
W_tilde <- (W_tilde + t(W_tilde))/2


### Eigenstuff

EV <- eigs_sym(W_tilde, k+1, 'LM') # first k+1 eigenpairs
V <- EV$vector
lambda <- EV$value


# the first eigenvector is the noise vector; 2+ describe the clusters

V <- V[,2:(k+1)]

U <-  matrix(rep(lambda[2:(k+1)], each=n), ncol=k)^(t)* V

U <- U/sqrt(rowSums(U^2))

clusters.DiffMap <- kmeans(U, centers=k, nstart = 100)
clusters.DiffMap <- clusters.DiffMap$clus
clusters <- clusters.DiffMap



#####
#####
#####
##### Plotting #####


# plot dimensions 1 vs 2, and 3 vs 4

plot(U[,1], U[,2], col=clusters.DiffMap, pch=16,
    xlab = "Eigenvector 1",
    ylab = "Eigenvector 2")

plot(U[,3], U[,4], col=clusters.DiffMap, pch=16,
    xlab = "Eigenvector 3",
    ylab = "Eigenvector 4")


plot(U[,1], U[,1], col=clusters.DiffMap, pch=16)
plot(U[,2], U[,2], col=clusters.DiffMap, pch=16)
plot(U[,3], U[,3], col=clusters.DiffMap, pch=16)
plot(U[,4], U[,4], col=clusters.DiffMap, pch=16)


# Pairs plot

pairs(U[,1:3], col=clusters.DiffMap)

pairs(U[,1:4], col=clusters.DiffMap)


# 3D plot

library(rgl)
plot3d(U[,1], U[,2], U[,3], col=clusters.DiffMap, box=F, add=F)


# Translate to the first quadrant for easier visualization

test <- U - min(U)
plot3d(test[,1], test[,2], test[,3], col=clusters.DiffMap, 
    xlim=c(0,1.6), ylim=c(0,1.6), zlim=c(0,1.6), box=F, add=F)


plot3d(test[,1], test[,2], test[,3], col=clusters.DiffMap, 
    xlim=c(0,2), ylim=c(0,2), zlim=c(0,2), box=F, add=F)


#####
#####
#####
}
#####
#####
#####
##### Analyze Clusters with the Scaled Validation Data #####


###
### Parallel Coordinates Plot
###

clusters <- clusters.DiffMap

mean1 <- colMeans(data.imp[clusters==1,])
mean2 <- colMeans(data.imp[clusters==2,])
mean3 <- colMeans(data.imp[clusters==3,])
mean4 <- colMeans(data.imp[clusters==4,])


groupmeans <- rbind(mean1, mean2, mean3, mean4)
cluster <- as.factor(c(1:4))
groupmeans <- data.frame(groupmeans,cluster )
library(MASS)
library(GGally)


# Plot with standardized axes (all scaled to [0,1])
ggparcoord(data = groupmeans, columns = 1:9, scale = "uniminmax", 
    mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


# Plot with NON-standardized axes (only scaled by standard deviation)
ggparcoord(data = groupmeans, columns = 1:9, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


# Plot without the "Gen" variables since they are much noisier than the other 2 tests

temp <- groupmeans[,-c(1,4,7)]
ggparcoord(data = temp, columns = 1:6, scale = "std", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)

temp <- groupmeans[,-c(1,4,7, 2,5,8)]
temp <- groupmeans[,-c(1,4,7, 3,6,9)]
temp <- groupmeans[,-c(3,6,9, 2,5,8)]

ggparcoord(data = temp, columns = 3:1, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


###
### Boxplots
###

# windows(20,20)
# par(mfrow=c(3,3))
# boxplot(gen12m~clusters, data=data.imp, main = "gen12m")
# boxplot(vasl12m~clusters, data=data.imp, main = "vasl12m")
# boxplot(rmprop12m~clusters, data=data.imp, main = "rmprop12m")
# boxplot(gen3m~clusters, data=data.imp, main = "gen3m")
# boxplot(vasl3m~clusters, data=data.imp, main = "vasl3m")
# boxplot(rmprop3m~clusters, data=data.imp, main = "rmprop3m")
# boxplot(gen2w~clusters, data=data.imp, main = "gen2w")
# boxplot(vasl2w~clusters, data=data.imp, main = "vasl2w")
# boxplot(rmprop2w~clusters, data=data.imp, main = "rmprop2w")
# 
# par(mfrow=c(1,1))



#####
#####
#####
##### Use the Unscaled Validation Data for True Values #####

###
### Recalculate the validation variables, 
### but scale MANUALLY (to be unscaled at the end)
###

dataw <- originalP[,2:10]

bad13 <- which(is.na(dataw[,1]) & is.na(dataw[,2]) & is.na(dataw[,3])) # length = 237

bad46 <- which(is.na(dataw[,4]) & is.na(dataw[,5]) & is.na(dataw[,6])) # length = 195

bad79 <- which(is.na(dataw[,7]) & is.na(dataw[,8]) & is.na(dataw[,9])) # length = 155

badrows <- c(bad13,bad46,bad79)

data.comp <- dataw[-badrows,]

means <- colMeans(data.comp, na.rm=T)
means.mat <- matrix(rep(means, 618), ncol=9, byrow=T)
sdevs <- apply(data.comp, 2, function(x) sqrt(var(x, na.rm=T)))
sdevs.mat <- matrix(rep(sdevs, 618), ncol=9, byrow=T)

data.comp <- (data.comp - means.mat)/sdevs.mat

data13 <- data.comp[,1:3]
data46 <- data.comp[,4:6]
data79 <- data.comp[,7:9]

# KNN imputation

library(VIM)

data13 <- kNN(data13, k=3)[,1:3]
data46 <- kNN(data46, k=3)[,1:3]
data79 <- kNN(data79, k=3)[,1:3]

data.imp.noscale <- cbind(data13, data46, data79)

data.imp.noscale <- (data.imp.noscale*sdevs.mat)+means.mat


###
### Inspect clusters
###

clusters <- clusters.DiffMap

mean1 <- colMeans(data.imp.noscale[clusters==1,])
mean2 <- colMeans(data.imp.noscale[clusters==2,])
mean3 <- colMeans(data.imp.noscale[clusters==3,])
mean4 <- colMeans(data.imp.noscale[clusters==4,])


groupmeans <- rbind(mean1, mean2, mean3, mean4)
cluster <- as.factor(c(1:4))
groupmeans <- data.frame(groupmeans,cluster )
library(MASS)
library(GGally)


# Parallel Coordinates Plot
ggparcoord(data = groupmeans, columns = 9:1, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


temp <- groupmeans[,-c(1,4,7, 2,5,8)]
temp <- groupmeans[,-c(1,4,7, 3,6,9)]
temp <- groupmeans[,-c(3,6,9, 2,5,8)]

ggparcoord(data = temp, columns = 3:1, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)



#####
#####
#####
##### Principal Component Exploration of the Real Data #####


clusters <- clusters.DiffMap

# First Eigenvector of cluster 1

data.valid1 <- Project[clusters==1,]
valid1.cor <- cor(data.valid1)
bad1 <- which(is.na(valid1.cor[,1]))
if (length(bad1) != 0) {
    valid1.cor[bad1,] <- 0
    valid1.cor[,bad1] <- 0
    valid1.cor[bad1,bad1] <- 1
}
valid1.eig <- eigs_sym(valid1.cor, 1, 'LM')
vars1 <- which(abs(valid1.eig$vector) > sort(abs(valid1.eig$vector))[.9*length(valid1.eig$vector)])
# colnames(valid1.cor)[vars1]


# First Eigenvector of cluster 2

data.valid2 <- Project[clusters==2,]
valid2.cor <- cor(data.valid2)
bad2 <- which(is.na(valid2.cor[,1]))
if (length(bad2) != 0) {
    valid2.cor[bad2,] <- 0
    valid2.cor[,bad2] <- 0
    valid2.cor[bad2,bad2] <- 1
}
valid2.eig <- eigs_sym(valid2.cor, 1, 'LM')
vars2 <- which(abs(valid2.eig$vector) > sort(abs(valid2.eig$vector))[.9*length(valid2.eig$vector)])
# colnames(valid2.cor)[vars2]


# First Eigenvector of cluster 3

data.valid3 <- Project[clusters==3,]
valid3.cor <- cor(data.valid3)
bad3 <- which(is.na(valid3.cor[,1]))
if (length(bad3) != 0) {
    valid3.cor[bad3,] <- 0
    valid3.cor[,bad3] <- 0
    valid3.cor[bad3,bad3] <- 1
}
valid3.eig <- eigs_sym(valid3.cor, 1, 'LM')
vars3 <- which(abs(valid3.eig$vector) > sort(abs(valid3.eig$vector))[.9*length(valid3.eig$vector)])
# colnames(valid3.cor)[vars3]


# First Eigenvector of cluster 4

data.valid4 <- Project[clusters==4,]
valid4.cor <- cor(data.valid4)
bad4 <- which(is.na(valid4.cor[,1]))
if (length(bad4) != 0) {
    valid4.cor[bad4,] <- 0
    valid4.cor[,bad4] <- 0
    valid4.cor[bad4,bad4] <- 1
}
valid4.eig <- eigs_sym(valid4.cor, 1, 'LM')
vars4 <- which(abs(valid4.eig$vector) > sort(abs(valid4.eig$vector))[.9*length(valid4.eig$vector)])
# colnames(valid4.cor)[vars4]


# List of all the important variables

    # vars <- unique(c(vars1, vars2, vars3, vars4))


# Are any important variables duplicated?

    # table(c(vars1, vars2, vars3, vars4))


# Which variables are common across clusters?

commons <- names(which(table(c(vars1, vars2, vars3, vars4))>1))
commons <- as.numeric(commons)


# Which variables UNIQUELY identify each cluster?

vars1.good <- setdiff(vars1,intersect(vars1,commons))
names1 <- colnames(Project)[vars1.good]
names1
bad1
eigv1 <- valid1.eig$vector
eigv1[vars1.good]


vars2.good <- setdiff(vars2,intersect(vars2,commons))
names2 <- colnames(Project)[vars2.good]
names2
bad2
eigv2 <- valid2.eig$vector
eigv2[vars2.good]


vars3.good <- setdiff(vars3,intersect(vars3,commons))
names3 <- colnames(Project)[vars3.good]
names3
bad3
eigv3 <- valid3.eig$vector
eigv3[vars3.good]



vars4.good <- setdiff(vars4,intersect(vars4,commons))
names4 <- colnames(Project)[vars4.good]
names4
bad4
eigv4 <- valid4.eig$vector
eigv4[vars4.good]


#####
#####
#####
##### Explore Cluster Means & Variances #####

clusters <- clusters.DiffMap

ProjectS <- scale(Project, center=TRUE, scale=FALSE)

# Mean analysis of cluster 1

data.valid1 <- as.matrix(ProjectS[clusters==1,])

means.1 <- colMeans(data.valid1)
means.1a <- abs(means.1)
vars1 <- which(means.1a > sort(means.1a)[.9*length(means.1a)])

sort(means.1[vars1])


# Mean analysis of cluster 2

data.valid2 <- as.matrix(ProjectS[clusters==2,])

means.2 <- colMeans(data.valid2)
means.2a <- abs(means.2)
vars2 <- which(means.2a > sort(means.2a)[.9*length(means.2a)])

sort(abs(means.2[vars2]), decr=T)

sort(means.2[vars2])



# Mean analysis of cluster 3

data.valid3 <- as.matrix(ProjectS[clusters==3,])

means.3 <- colMeans(data.valid3)
means.3a <- abs(means.3)
vars3 <- which(means.3a > sort(means.3a)[.9*length(means.3a)])

sort(abs(means.3[vars3]), decr=T)

sort(means.3[vars3])



# Mean analysis of cluster 4

data.valid4 <- as.matrix(ProjectS[clusters==4,])

means.4 <- colMeans(data.valid4)
means.4a <- abs(means.4)
vars4 <- which(means.4a > sort(means.4a)[.9*length(means.4a)])

sort(abs(means.4[vars4]), decr=T)

sort(means.4[vars4])


#####
#####
##### 
##########      Spectral Clustering on the Entire Dataset
{
    #####
    #####
    #####
    ##### Correlation Similarity Measure #####
    
    
    Similarity <- corrsim(Project.full)
    
    
    ##### 
    ##### 
    ##### 
    ##### Clustering #####
    
    k <- 3
    
    t = .5  # "fuzziness" parameter
    
    Weights <- Similarity 
    n <- nrow(Weights)
    
    # # Row/Column symmetric weighting Process
    # dvec_inv = 1/sqrt(rowSums(Weights))
    # # normalize the correlation matrix by the row lengths
    # W_tilde = Diagonal(n,dvec_inv) %*% Weights %*% Diagonal(n,dvec_inv)
    
    
    # Probability distribution L1 weighting
    dvec_inv = 1/rowSums(Weights)
    # normalize the correlation matrix by the row lengths
    W_tilde = diag(dvec_inv) %*% Weights
    W_tilde <- (W_tilde + t(W_tilde))/2
    
    EV <- eigs_sym(W_tilde, k+1, 'LM') # first k+1 eigenpairs
    V <- EV$vector
    lambda <- EV$value
    
    # the first eigenvector is the noise vector; 2+ describe the clusters
    
    V <- V[,2:(k+1)]
    
    U <-  matrix(rep(lambda[2:(k+1)], each=n), ncol=k)^(t)* V
    
    U <- U/sqrt(rowSums(U^2))
    
    clusters.DiffMap <- kmeans(U, centers=k, nstart = 100)
    clusters.DiffMap <- clusters.DiffMap$clus
    
    
    
    
    #####
    #####
    #####
    ##### Plotting #####
    
    
    # Pairs plot
    
    pairs(U[,1:3], col=clusters.DiffMap)
    
    
    # 3D plot
    
    library(rgl)
    plot3d(U[,1], U[,2], U[,3], col=clusters.DiffMap, box=F, add=F)
    
    
    # Translate to the first quadrant for easier visualization
    
    test <- U - min(U)
    plot3d(test[,1], test[,2], test[,3], col=clusters.DiffMap, 
        xlim=c(0,1.6), ylim=c(0,1.6), zlim=c(0,1.6), box=F, add=F)
    #####
    #####
    #####
}
#####
#####
##### Analyze our clusters with the validation data #####


###
### Parallel Coordinates Plot
###

clusters <- clusters.DiffMap[-badrows]

mean1 <- colMeans(data.imp[clusters==1,])
mean2 <- colMeans(data.imp[clusters==2,])
mean3 <- colMeans(data.imp[clusters==3,])
mean4 <- colMeans(data.imp[clusters==4,])


groupmeans <- rbind(mean1, mean2, mean3, mean4)
cluster <- as.factor(c(1:4))
groupmeans <- data.frame(groupmeans,cluster )
library(MASS)
library(GGally)


# Plot with standardized axes (all scaled to [0,1])
ggparcoord(data = groupmeans, columns = 1:9, scale = "uniminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


# Plot with NON-standardized axes (only scaled by standard deviation)
ggparcoord(data = groupmeans, columns = 1:9, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


# Plot without the "Gen" variables since they are much noisier than the other 2 tests

temp <- groupmeans[,-c(1,4,7)]
ggparcoord(data = temp, columns = 1:6, scale = "std", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)

temp <- groupmeans[,-c(1,4,7, 2,5,8)]
temp <- groupmeans[,-c(1,4,7, 3,6,9)]
temp <- groupmeans[,-c(3,6,9, 2,5,8)]

ggparcoord(data = temp, columns = 3:1, scale = "globalminmax", mapping=aes(color=as.factor(cluster)),
    title = "Parallel Coordinate plot: cluster structure")+
    scale_color_discrete("cluster",labels=groupmeans$cluster)+geom_line(size=1)


###
### Boxplots
###

# windows(20,20)
# par(mfrow=c(3,3))
# boxplot(gen12m~clusters, data=data.imp, main = "gen12m")
# boxplot(vasl12m~clusters, data=data.imp, main = "vasl12m")
# boxplot(rmprop12m~clusters, data=data.imp, main = "rmprop12m")
# boxplot(gen3m~clusters, data=data.imp, main = "gen3m")
# boxplot(vasl3m~clusters, data=data.imp, main = "vasl3m")
# boxplot(rmprop3m~clusters, data=data.imp, main = "rmprop3m")
# boxplot(gen2w~clusters, data=data.imp, main = "gen2w")
# boxplot(vasl2w~clusters, data=data.imp, main = "vasl2w")
# boxplot(rmprop2w~clusters, data=data.imp, main = "rmprop2w")
# 
# par(mfrow=c(1,1))



#####
#####
#####
#####
#####
#####


