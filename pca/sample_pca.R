rm(list=ls())
setwd("E:/R Code/meetup/pca")
library(readbitmap)
library(png)
library(jpeg)


data(iris)
str(iris)

# ####################### Picture example
img1 <- read.bitmap(f="./logo.jpg",channel=1,IdentifyByExtension=T)
d1 <- dim(img1)
#d1
img2 <- sapply(img1,round)
img3 <- matrix(img2,nrow=d1[1],ncol=d1[2])
image(t(img3))
# dimensions
d1[2]
dim(img3)
ddim <- d1[1]*d1[2]
d_rec <- as.integer(d1[2]/10)
d_rec
#d_rec <- 20

sv2 <- svd(img3)
plot(sv2$d^2/sum(sv2$d^2),col="blue",pch=19)
lines(sv2$d^2/sum(sv2$d^2),col="black")
i <- sv2$d[1:15]
barplot(i/sum(i),col="blue",names.arg = paste(rep("PC",15),1:15,sep = ""))

r3 <- sv2$u[,1:32] %*% diag(sv2$d[1:32]) %*% t(sv2$v[,1:32])
image(r3)

################################



# iris data
pairs(iris[,-5],col=iris[,5],cex=0.8,cex.labels = 0.86)

### PCA like - svd
sv1 <- svd(iris[,-5])

# scree plot
plot(sv1$d^2/sum(sv1$d^2),col="blue",pch=19)
lines(sv1$d^2/sum(sv1$d^2),col="black")
barplot(sv1$d^2/sum(sv1$d^2),col="blue",names.arg = paste(rep("PC",4),1:4,sep = ""))

## Visualizing U and V
dim(sv1$u)
dim(sv1$v)
sv1$v

plot(1:150,sv1$u[,1],pch=19,col=iris[,5])
plot(1:150,sv1$u[,2],pch=19,col=iris[,5])
plot(1:150,sv1$u[,3],pch=19,col=iris[,5])
plot(1:150,sv1$u[,4],pch=19,col=iris[,5])

barplot(sv1$v[,1],pch=19,names.arg=colnames(iris[,-5]))
barplot(sv1$v[,2],pch=19,names.arg=colnames(iris[,-5]))
barplot(sv1$v[,3],pch=19,names.arg=colnames(iris[,-5]))
barplot(sv1$v[,4],pch=19,names.arg=colnames(iris[,-5]))

# returing back data
r1 <- sv1$u[,1:2] %*% diag(sv1$d[1:2]) %*% t(sv1$v[,1:2])
plot(iris[,1],r1[,1])
plot(iris[,4],r1[,4])

### prcomp package ##############
pca1 <- prcomp(iris[,-5],center=T,scale=T)


print(pca1)
pca1$x
head(pca1$x)

# Plotting 
plot(pca1)
plot(pca1$x[,c(1,2)],col=iris[,5],pch=19)

barplot(pca1$sdev^2,names.arg = paste(rep("PC",4),1:4,sep = ""))

## Plotting the first two components
plot(pca1$x[,c(1,2)],col=iris[,5],pch=19)

## How to get the amount of variabality in each priniciple component
pca1$sdev^2
cumsum(pca1$sdev^2)/sum(pca1$sdev^2)


### Compressing
pca2 <- prcomp(iris[,-5],center = T,scale=T)

pca2 <- prcomp(iris[,-5],center = FALSE,scale=FALSE)
cumsum(pca2$sdev^2)/sum(pca2$sdev^2)

# return back data
rr <- pca2$x[,1:2] %*% t(pca2$rotation[,1:2])

#Check the real data versus the compressed
plot(rr[,1],iris[,1])
plot(rr[,2],iris[,2])
plot(rr[,3],iris[,3])
plot(rr[,4],iris[,4])

# ####################### FactoMineR
require(FactoMineR)
  
res <- PCA(iris,quali.sup=5,graph=T)

## When scalling isn't required
res2 <- PCA(iris,quali.sup=5,graph=T, scale.unit = F) 

# Just take into account that scaling is important if you want 
# every column to be treated equally
#

res <- PCA(iris, quali.sup=5,graph=F)
# Examining output
res$eig

barplot(res$eig[,2],names.arg = row.names(res$eig),main="Variance explained by each component")

res$var
# Contains a list of 4 data.frames
# first $coord

# printing the results of the PCA
print(res)

## Different effects
# The individuals can be colored according to a categorical variable 
# in the individual graph. To do so, the following code is used:
 
plot(res,habillage = 5)

# plot variables with high projection limit
plot(res, choix = "var", axes = c(2, 3))

plot(res,choix=c("var"))
plot(res,choix=c("ind"))
plot(res,choix=c("axes"))

# ROWS -> BLUE |||| COLUMNS -> RED

#Active Rows is in default the blue
#Supplementaty Rows are in darker blue and in itallic
#Active columns are in bright red
#Supplementaty Columns are in darker red and in itallic


# We can describe each principal component using the dimdesc function:
dimdesc(res)

# The variables are kept if the p value is less than 0.20 (proba = 0.2)
dimdesc(res,proba = 0.2)

# ####################  correspondence analysis
data("children")
# The columns from 6 to 8 are supplementaries (they concern the age groups of the people),
# and rows from 15 to 18 are either supplementaries. 
# By default, the CA function gives one graphical output (Figure 5).

res.ca <- CA(children, col.sup = 6:8, row.sup = 15:18)

#If we just want to visualize the active elements only
plot(res.ca, invisible = c("row.sup", "col.sup"))


# ######################### Multiple Factor Analysis (MFA)
# Let us define two groups
# Group 1:  Sepal and contains two quantitative variables that we need to scale. (column 1 and 2):
#   Sepal.Length
#   Sepal.Width
# Group 2: Petal and  contains two quantitative variables that we need to scale. (column 3 and 4)
#  Petal.Length
#  Petal.Width
# Group 3: Species and it contain only one qualitative variable (column 5)
#  Species

res_mfa <- MFA(iris,group=c(2,2,1),type=c("s","s","n"),name.group=c("Sepal","Petal","Species"),graph=T)


# If we want to identify the third group as supplementary we add 
# the following argument num.group.sup  which the index of the group 
# you want to mark as supplementary.
res_mfa <- MFA(iris,group=c(2,2,1),type=c("s","s","n"),name.group=c("Sepal","Petal","Species"),num.group.sup = 3)

plot(res_mfa,choix=c("var"))
plot(res_mfa,choix=c("ind"),habillage = 5)
plot(res_mfa,choix=c("group"))

### Returning back original data
# For scalled data
r2 <- res$svd$U %*% diag(res$svd$vs) %*% t(res$svd$V)
v1 <- diag(res$svd$vs) %*% t(res$svd$V)
# gets only the data based on the first two priniciple components
r2 <- res$svd$U[,c(1,2)] %*% v1[1:2,]

# For MFA Groups
r2 <- res_mfa$separate.analyses$Sepal$svd$U %*% t(res_mfa$separate.analyses$Sepal$svd$V)

res_not_scalled <- MFA(iris,group=c(2,2,1),type=c("c","c","n"),name.group=c("Sepal","Petal","Species"),num.group.sup = 3)
# For not scalled data
r2 <- res_not_scalled$separate.analyses$Sepal$svd$U %*% 
  diag(res_not_scalled$separate.analyses$Sepal$svd$vs) %*% 
  t(res_not_scalled$separate.analyses$Sepal$svd$V)
#or 

v1 <- diag(res_not_scalled$separate.analyses$Sepal$svd$vs) %*% t(res_not_scalled$separate.analyses$Sepal$svd$V)
# gets only the data based on the first two priniciple components
r2 <- res_not_scalled$separate.analyses$Sepal$svd$U[,c(1,2)] %*% v1[1:2,]

plot(iris[,2],r2[,2])

## For unscalled data
#Non scaled 

l1 <- dim(res_mfa$separate.analyses$Sepal$svd$U)[2]
b_ret <- res_mfa$separate.analyses$Sepal$svd$U %*% diag(res_mfa$separate.analyses$Sepal$svd$vs[1:l1]) %*% t(res_mfa$separate.analyses$Sepal$svd$V)

plot(iris[,1],b_ret[,1])

#### GUI Rcmdr
require(Rcmdr)
