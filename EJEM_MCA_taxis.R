##########################################################################
#####											   #####
#####											   #####
#####     MCA                    #####
#####     in FactoMineR package 						   #####
#####											   #####
#####                                                                #####
##########################################################################
rm(list=ls())


### Load FactoMineR ###
library(FactoMineR)


### Read datadf4 ###
base<-read.csv2("taxis_fin.csv",row.names=1,header=TRUE,dec=",")
dim(base)
colnames(base)
summary(base)
resultat<-boxplot(df4$Total_amount)
resultat <- boxplot(df4$Total_amount)      # Identifying outliers
names(resultat)
valaberrante<-resultat$out                    # These are the outlier values
which(df4$Total_amount %in% valaberrante)
base<-base[-which(base$Total_amount %in% valaberrante),]
dim(base)

colnames(base)
### Remember, in PCA, the active variables were: #######
# ACTIVAS: Trip_distance ,Fare_amount, Extra, MTA_tax, Tip_amount,
# Tolls_amount, improvement_surcharge


####    Discretization of Total Amount
#  discretization: 4 levels are considered, from the quartiles
decoupe<-quantile (df4$Total_amount,na.rm=TRUE)
decoupe[1]<-decoupe[1]-1e-10
df4$Tamount_inclass<-cut(df4$Total_amount,breaks=decoupe)
summary(df4$Tamount_inclass)
df4[is.na(df4$Total_amount),]
levels(df4$Tamount_inclass)<-c("Very_small_amount","small_amount",
                              "Moderate_amount","High_amount")

# In this case, discretization: 4 levels are considered, from the quartiles
decoupe<-quantile (df4$Trip_distance ,na.rm=TRUE)
decoupe[1]<-decoupe[1]-1e-10
df4$Trip_distance_inclass<-cut(df4$Trip_distance,breaks=decoupe)
summary(df4$Trip_distance_inclass)
df4[is.na(df4$Trip_distance),]
levels(df4$Trip_distance_inclass)<-c("Very_small_dist","small_dist",
                              "Moderate_dist","High_dist")

# In this case, discretization: 4 levels are considered, from the quartiles
decoupe<-quantile (df4$Fare_amount ,na.rm=TRUE)
decoupe[1]<-decoupe[1]-1e-10
df4$Fare_amount_inclass<-cut(df4$Fare_amount,breaks=decoupe)
summary(df4$Fare_amount_inclass)
df4[is.na(df4$Fare_amount),]
levels(df4$Fare_amount_inclass)<-c("Very_small_fare","small_fare",
                              "Moderate_fare","High_fare")


summary(as.factor(df4$Extra))
df4$Extra_fac<-as.factor(df4$Extra)
levels(df4$Extra_fac)<-c("extra_0","extra_0.5","extra_1")
summary(df4$Extra_fac)
summary(as.factor(df4$MTA_tax))   # inutilizable
summary(as.factor(df4$Tolls_amount))   ## inutilizable
summary(as.factor(df4$improvement_surcharge))   # inutilizable
summary(df4$Trip_type)  # inutilizable


#Apply MCA to the data frame with the following columns: "Total_amount",
#"Tamount_inclass" , "Trip_distance_inclass", "Fare_amount_inclass" , "Extra_fac" and
#"MTA_tax_fac"

################# MCA
colnames(df4)
colnames(df4[,c(16, 21, 23:25)])
res.mca<-MCA(df4[,c(16, 21, 23:25) ],quanti.sup=1,quali.sup=c(3))
colnames(df4)


### I. Eigenvalues and dominant axes. How many axes we have to interpret? ###
names(res.mca)
summary(res.mca)
round(res.mca$eig,3)
barplot(res.mca$eig[,1],main="valores propios",names.arg=paste("dim",1:nrow(res.mca$eig)))
sum(res.mca$eig[,1])


### II.  Individuals point of view
### Are they any individuals "too contributive"       ##############

### III. Interpreting the axes:  Variables point of view
### coordinates, quality of representation, contribution of the variables  ##############
### 
round(cbind(res.pca$var$coord[,1:3],res.pca$var$cos2[,1:3],res.pca$var$contrib[,1:3]),2)
round(cbind(res.pca$var$cos2[,1:4],res.pca$var$contrib[,1:4]),2)
# dimdes easies this description from the variables
dimdesc(res.mca)
###
names(res.mca)
res.mca$var$coord
res.mca$quali.sup$coord

# representación de los individuos
plot.MCA(res.mca,choix=c("ind"),cex=0.8)
plot.MCA(res.mca,choix=c("ind"),invisible=c("var","quali.sup"),cex=0.8)

# representación de las categorías
plot.MCA(res.mca,choix=c("ind"),invisible=c("ind"),axes=c(1,2))
lines(res.mca$var$coord[4:8,1],res.mca$var$coord[4:8,2],lwd=2,col="blue")
lines(res.mca$var$coord[6:9,1],res.mca$var$coord[6:9,2],lwd=1,col="green")
lines(res.mca$var$coord[10:13,1],res.mca$var$coord[10:13,2],lwd=1,col="black")
lines(res.mca$quali.sup$coord[1:5,1],res.mca$quali.sup$coord[1:5,2],lwd=2,col="magenta")
names(res.mca)
res.mca$var
plot.MCA(res.mca,choix=c("ind"),invisible=c("ind"),axes=c(3,4))
plot.MCA(res.mca,choix=c("var"),axes=c(3,4))


