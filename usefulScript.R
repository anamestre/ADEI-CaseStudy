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
boxplot(df$Passenger_count)

#histograms to do:
hist(df$Pickup_latitude, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Pickup_longitude, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Dropoff_latitude, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Dropoff_longitude, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Trip_distance, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Fare_amount, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Extra, main = NULL, xlab = NULL, ylab = NULL)
hist(df$MTA_tax, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Tip_amount, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Tolls_amount, main = NULL, xlab = NULL, ylab = NULL)
hist(df$improvement_surcharge, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Total_amount, breaks = 40, main = NULL, xlab = NULL, ylab = NULL)
hist(df$Passenger_count, main = NULL, xlab = NULL, ylab = NULL)

#barplots for factors:
pickupCounts <- table(df$pickup)
barplot(pickupCounts)
dropoffCounts <- table(df$dropoff)
barplot(dropoffCounts)
triptypeCounts <- table(df$Trip_type)
barplot(triptypeCounts)
paytypeCounts <- table(df$Payment_type)
barplot(paytypeCounts)
ratecodeCounts <- table(df$RateCodeID)
barplot(ratecodeCounts)
storefwdCounts <- table(df$Store_and_fwd_flag)
barplot(storefwdCounts)
vendorCounts <- table(df$VendorID)
barplot(vendorCounts)


df$pickup <- as.factor(df$pickup)
df$dropoff <- as.factor(df$dropoff)
#aqu? va un summary de totes dades
#abans dels summary: sink("arxiu.txt")
#després dels summary: sink()

df$NAvalues <- 0


sel<-rownames(df[which(df[,"Pickup_longitude"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Pickup_longitude"]<- "missing"
 sel<-rownames(df[which(df[,"Pickup_latitude"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Pickup_latitude"]<- "missing"
 sel<-rownames(df[which(df[,"Dropoff_latitude"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Dropoff_latitude"]<- "missing"
 sel<-rownames(df[which(df[,"Dropoff_longitude"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Dropoff_longitude"]<- "missing"
 sel<-rownames(df[which(df[,"Passenger_count"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Passenger_count"]<- "missing"
 sel<-rownames(df[which(df[,"Trip_distance"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Trip_distance"]<- "missing"
 sel<-rownames(df[which(df[,"Fare_amount"]==0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Fare_amount"]<- "missing"
 sel<-rownames(df2[which(df2[,"Fare_amount"]<0),]) #seleccionamos aparte las que tienen longitud 0
 sel<-rownames(df[which(df[,"Fare_amount"]<0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Fare_amount"]<- "error"
 sel<-rownames(df[which(df[,"Extra"]<0),]) #seleccionamos aparte las que tienen longitud 0
 df[sel, "Extra"]<- "error"

                                       sel<-rownames(df[which(df[,"Extra"]==4.5),]) #seleccionamos aparte las que tienen longitud 0
                                       df[sel, "Extra"]<- "outlier"
                                       sel<-rownames(df[which(df[,"MTA_tax"]<0),]) #seleccionamos aparte las que tienen longitud 0
                                       df[sel, "MTA_tax"]<- "error"
                                       sel<-rownames(df[which(df[,"improvement_surcharge"]<0),]) #seleccionamos aparte las que tienen longitud 0
                                      df[sel, "improvement_surcharge"]<- "error"
                                       sel<-rownames(df[which(df[,"Total_amount"]<0),]) #seleccionamos aparte las que tienen longitud 0
                                       df[sel, "Total_amount"]<- "error"
                                      

                                      sel<-rownames(df[which(df[,"Total_amount"]==0),]) #seleccionamos aparte las que tienen longitud 0
                                       df[sel, "Total_amount"]<- "missing"
sel<-rownames(df[which(df[,"Trip_distance"]>(8.6)),])
                                      


for (i in 1:5000) {
  for(j in 1:23) {
    if(identical(df2[i,j], "missing") || identical(df2[i,j], "error") || identical(df2[i,j], "outlier")){
      df2[i,]$NAvalues <- df2[i,]$NAvalues+1
    }
  }
}


for (i in 1:5000) {
  for(j in 1:23) {
    if(identical(df2[i,j], "missing") || identical(df2[i,j], "error") || identical(df2[i,j], "outlier")){
      df2[i,j] <- NA
    }
  }
}

df$Pickup_latitude <- as.numeric(df$Pickup_latitude)
df$Pickup_longitude <- as.numeric(df$Pickup_longitude)
df$Dropoff_latitude <- as.numeric(df$Dropoff_latitude)
df$Dropoff_longitude <- as.numeric(df$Dropoff_longitude)
df$Trip_distance <- as.numeric(df$Trip_distance)
df$Fare_amount <- as.numeric(df$Fare_amount)
df$Extra <- as.numeric(df$Extra)
df$MTA_tax <- as.numeric(df$MTA_tax)
df$improvement_surcharge <- as.numeric(df$improvement_surcharge)
df$Total_amount <- as.numeric(df$Total_amount)
df$Passenger_count <- as.numeric(df$Passenger_count)

