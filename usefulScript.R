#rm(list=ls())  
#borra todo lo que haya (lo que haya dónde? nadie lo sabe)

#df<-read.csv2("green_tripdata_2016-01.csv", row.names=1, header=TRUE, dec=".",sep ="," )
#df2<-read.table("green_tripdata_2016-01.csv", header=T,sep ="," )
#lee un csv

#dim(df2) = dimensiones del dataframe
#colnames(df2) = nombres de las columnas
#head(df2) = primeras filas
#summary(df2) = medias medianas etc de todos los datos
#sample(1:nrow(df2), 5000) = elige 5000 de ese conjunto
#sam <- as.vector(sort(sample(1:nrow(df2), 5000)))


df[which(df[,"Pickup_longitude"]==0),] #cuáles tienen longitud 0?
length(df[which(df[,"Pickup_longitude"]==0),df$Pickup_longitude]) #cuántas tienen longitud 0?
sel<-rownames(df[which(df[,"Pickup_longitude"]==0),]) #seleccionamos aparte las que tienen longitud 0
df[sel, "Pickup_longitude"]<- NA #de las seleccionadas, le ponemos NA a Pickup_longitude