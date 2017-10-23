# df<-read.table("green_tripdata_2016-01.csv", header=T,sep ="," )  #loading the data
set.seed(27041996)
sam<-as.vector(sort(sample(1:nrow(df),5000)))
df<-df[sam,] #selection of 5000 data rows

df$VendorID<-factor(df$VendorID,labels=c("Creative","VeriFone"))
df$Trip_type<-factor(df$Trip_type,labels=c("Street-hail","Dispatch"))
df$Payment_type<-factor(df$Payment_type,labels=c("Credit card","Cash","No charge","Dispute"))
df$RateCodeID<-factor(df$RateCodeID,labels=c("Standard","JFK","Newark","Negotiated")) #no trips to Nassau/Westchester

df$pickup<-substr(strptime(df$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), 12, 13)
df$dropoff<-substr(strptime(df$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 12, 13)
df$pickup <- as.factor(df$pickup)
df$dropoff <- as.factor(df$dropoff)
#recoding of pickup and dropoff times

#count of missing/error/outlier values
df$NAvalues <- 0

sel<-rownames(df[which(df[,"Pickup_longitude"]==0),])
df[sel, "Pickup_longitude"]<- "missing"
sel<-rownames(df[which(df[,"Pickup_latitude"]==0),])
df[sel, "Pickup_latitude"]<- "missing"
sel<-rownames(df[which(df[,"Dropoff_latitude"]==0),])
df[sel, "Dropoff_latitude"]<- "missing"
sel<-rownames(df[which(df[,"Dropoff_longitude"]==0),])
df[sel, "Dropoff_longitude"]<- "missing"
sel<-rownames(df[which(df[,"Passenger_count"]==0),])
df[sel, "Passenger_count"]<- "missing"
sel<-rownames(df[which(df[,"Trip_distance"]==0),])
df[sel, "Trip_distance"]<- "missing"
sel<-rownames(df[which(df[,"Fare_amount"]==0),]) 
df[sel, "Fare_amount"]<- "missing"
sel<-rownames(df[which(df[,"Fare_amount"]<0),]) 
df[sel, "Fare_amount"]<- "error"
sel<-rownames(df[which(df[,"Extra"]<0),])
df[sel, "Extra"]<- "error"
sel<-rownames(df[which(df[,"Extra"]==4.5),]) 
df[sel, "Extra"]<- "outlier"
sel<-rownames(df[which(df[,"MTA_tax"]<0),])
df[sel, "MTA_tax"]<- "error"
sel<-rownames(df[which(df[,"improvement_surcharge"]<0),]) 
df[sel, "improvement_surcharge"]<- "error"
sel<-rownames(df[which(df[,"Total_amount"]<0),]) 
df[sel, "Total_amount"]<- "error"
sel<-rownames(df[which(df[,"Total_amount"]==0),]) 
df[sel, "Total_amount"]<- "missing"
sel<-rownames(df[which(df[,"Trip_distance"]>(8.6)),])
df[sel, "Trip_distance"]<- "outlier"

for (i in 1:5000) {
  for(j in 1:23) {
    if(identical(df2[i,j], "missing") || identical(df2[i,j], "error") || identical(df2[i,j], "outlier")){
      df2[i,]$NAvalues <- df2[i,]$NAvalues+1
    }
  }
}

#conversion to NA
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

#boxplots:
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

#histograms:
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