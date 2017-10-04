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

# variable cuantitativa: histograma y boxplot
# variable cualitativa: barplot

df$pickup<-substr(strptime(df$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), 12, 13)
df$dropoff<-substr(strptime(df$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 12, 13)

na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)



#boxplots to do:
boxplot(df$Pickup_latitude)
boxplot(df$Pickup_latitude)
boxplot(df$Pickup_longitude)
boxplot(df$Dropoff_latitude)
boxplot(df$Dropoff_longitude)
boxplot(df$Trip_distance)
boxplot(df$Fare_amount)
boxplot(df$Extra)
boxplot(df$MTA_tax)
boxplot(df$Tip_amount)
boxplot(df$Tolls_amount)
boxplot(df$improvement_surcharge)
boxplot(df$Total_amount)

#histograms to do:
hist(df$Pickup_latitude)
hist(df$Pickup_latitude)
hist(df$Pickup_longitude)
hist(df$Dropoff_latitude)
hist(df$Dropoff_longitude)
hist(df$Trip_distance)
hist(df$Fare_amount)
hist(df$Extra)
hist(df$MTA_tax)
hist(df$Tip_amount)
hist(df$Tolls_amount)
hist(df$improvement_surcharge)
hist(df$Total_amount)

#barplots for factors:
pickupCounts <- table(df$pickup)
barplot(pickupCounts)
dropoffCounts <- table(df$dropoff)
barplot(dropoffCounts)
triptypeCounts <- table(df$Trip_type)
barplot(triptypeCounts)