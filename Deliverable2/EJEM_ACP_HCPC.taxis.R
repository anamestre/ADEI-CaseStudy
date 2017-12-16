##########################################################################
#####											   #####
#####											   #####
#####     Principal Component Analysis (PCA)                         #####
#####     in FactoMineR package 						   #####
#####											   #####
#####                                                                #####
##########################################################################
rm(list=ls())


### Load FactoMineR ###
library(FactoMineR)


### Read database ###
base<-read.csv2("taxis_fin.csv",row.names=1,header=TRUE,dec=",")
dim(base)
colnames(base)
summary(base)
resultat<-boxplot(base$Total_amount)
resultat <- boxplot(base$Total_amount)      # Identifying outliers
names(resultat)
valaberrante<-resultat$out                    # These are the outlier values
which(base$Total_amount %in% valaberrante)
base<-base[-which(base$Total_amount %in% valaberrante),]
dim(base)
boxplot(base$Total_amount)
lm(base$Total_amount ~ base$Trip_distance) # omitting intercept#
base$Total_dist<-base$Total_amount-3.209*base$Trip_distance
summary(base$Total_dist)
boxplot(base$Total_dist)
colnames(base)
base$dif<-base$Total_amount-base$Total_dist
summary(base$dif)
colnames(base)
pickup<-as.factor(base$pickup)
dropoff<-as.factor(base$dropoff)

### Perform a Principal Component Analysis (PCA) #######
?PCA 
dfPCA <- dfD4
res.pca<-PCA(dfPCA, quali.sup=c(1:3, 16:19),quanti.sup= c(4:15,20))
plot.PCA(res.pca,choix=c("var"),invisible=c("quanti.sup"))
plot.PCA(res.pca,choix=c("var"),invisible=c("var"))
plot.PCA(res.pca,choix=c("var"),invisible=c("quanti.sup"))
plot.PCA(res.pca,choix=c("ind"),invisible=c("ind"))
?plot.PCA

### I. Eigenvalues and dominant axes. How many axes we have to interpret? ###
names(res.pca)
summary(res.pca)
round(res.pca$eig,3)
barplot(res.pca$eig[,1],main="valores propios",names.arg=paste("dim",1:nrow(res.pca$eig)))
summary(res.pca)
sum(res.pca$eig[,1])


### II.  Individuals point of view
### Are they any individuals "too contributive"       ##############
names(res.pca$ind)
round(cbind(res.pca$ind$coord[,1:3],res.pca$ind$cos2[,1:3],res.pca$ind$contrib[,1:3]),2)

# To better understand the axes through the extreme individuals
res.pca$ind$coord
rango<-order(res.pca$ind$coord[,1])
length(rango)
row.names(df4)[rango[4660:4669]]
row.names(df4)[rango[1:10]]
df4[which(row.names(df4)=="1329730"),1:22]
df4[which(row.names(df4)=="363607"),1:22]
####
### III. Interpreting the axes:  Variables point of view
### coordinates, quality of representation, contribution of the variables  ##############
### 
round(cbind(res.pca$var$coord[,1:3],res.pca$var$cos2[,1:3],res.pca$var$contrib[,1:3]),2)
round(cbind(res.pca$var$cos2[,1:4],res.pca$var$contrib[,1:4]),2)
# dimdes easies this description from the variables
dimdesc(res.pca)
###

### we can need more than 2 axes to have a good representation of the clouds
?plot.PCA
plot.PCA(res.pca,choix=c("ind"),cex=0.8)
plot.PCA(res.pca,choix=c("ind"),invisible=c("quali"),axes=c(3,4))
plot.PCA(res.pca,choix=c("var"),axes=c(3,4))

### IV. Perform a PCA taking into account also supplementary variables
### the supplementary variables can be quantitative and/or categorical  
#res.pca <- PCA(df4,quanti.sup=c(2,13,15,18),quali.sup=c(16,17,19),ncp=4) (cal arreglar això)
res.pca<-PCA(df4, quali.sup=c(1,2,3,17,18,19,20,21,22, 23),quanti.sup= c(4:8,10,13,14,16), ncp=2)
summary(res.pca)

plot(res.pca, choix="ind",invisible="ind")
lines(res.pca$quali.sup$coord[6:8,1],res.pca$quali.sup$coord[6:8,2],lwd=2,col="black")
res.pca$quali.sup$coord


###  Variables point of view
dimdesc(res.pca)


###### plots
plot(res.pca, choix="ind", invisible="quali")
plot(res.pca, choix="ind", invisible="ind")
plot(res.pca, choix="var",invisible=c("row","col","quanti.sup"))
plot(res.pca, choix="var",invisible="quanti.sup",axes=1:2)




# FInalmente, Synthesis through HCPC
###
### Clustering the individuals
### Before, hou have to perform a PCA with the number of axes 
### that you have decided to take into account (indicated through ncp=)
?HCPC

#  Con reasignaciones o no?  Por defecto, hay iteraciones de consolidación
#res.pca<-PCA(df4, quali.sup=c(1,2,16,17,18,19,20),quanti.sup= c(3:6,15,21,22),ncp=2,graph=FALSE) 
res.pca<-PCA(df4, quali.sup=c(1,2,3,17,18,19,20,21,22, 23),quanti.sup= c(4:8,10,13,14,16), ncp=2, graph = FALSE)
?HCPC
res.hcpc<-HCPC(res.pca,order=TRUE, nb.clust = -1)
names(res.hcpc)
### Interpretar los resultados de la clasificación

res.hcpc$desc.axes
res.hcpc$desc.var 
res.hcpc$desc.ind 

# Examinar los valores de los individuos que caracterizan a las clases



# FInalmente, Synthesis through HCPC
###
### Clustering the individuals
### Before, hou have to perform a PCA with the number of axes 
### that you have decided to take into account (indicated through ncp=)

#  Con reasignaciones o no?  Por defecto, hay iteraciones de consolidación
res.pca<-PCA(base,
quali.sup=c(1,2,16,17,18,19,20),quanti.sup= c(3:6,15,21,22),ncp=6,graph=FALSE) 
?HCPC
res.hcpc<-HCPC(res.pca,order=TRUE)
names(res.hcpc)
#res.hcpc<-HCPC(res.pca,min=8,max=12)
### Interpretar los resultados de la clasificación


summary(res.hcpc$data.clust$clust)


### desc.var ###
### A. The description of the clusters by the variables ###
names(res.hcpc$desc.var)

### desc.var$test.chi2 ###
### A.1. The categorical variables which characterizes the clusters ###
res.hcpc$desc.var$test.chi2

### desc.var$category ###
### A.2. The description of each cluster by the categories ##
res.hcpc$desc.var$category

### desc.var$quanti.var ###
### A.3. The quantitative variables which characterizes the clusters ###
res.hcpc$desc.var$quanti.var

### desc.var$quanti ###
### A.4. The description of each cluster by the quantitative variables ###
res.hcpc$desc.var$quanti

### desc.axes ###
### B. The description of the clusters by the axes ###
names(res.hcpc$desc.axes)
res.hcpc$desc.axes$quanti.var
res.hcpc$desc.axes$quanti

### desc.ind ###
### C. The description of the clusters by the individuals ###
names(res.hcpc$desc.ind)
res.hcpc$desc.ind$para
res.hcpc$desc.ind$dist

# Examinar los valores de los individuos que caracterizan a las clases


####
#### THE END
####
 



