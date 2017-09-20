df<-read.table("green_tripdata_2016-01.csv", header=T,sep ="," )  #loading the data
set.seed(27041996)
sam<-as.vector(sort(sample(1:nrow(df),5000)))
df<-df[sam,] #selection of 5000 data rows

df$VendorID<-factor(df$VendorID,labels=c("Creative","VeriFone"))
df$Trip_type<-factor(df$Trip_type,labels=c("Street-hail","Dispatch"))
df$Payment_type<-factor(df$Payment_type,labels=c("Credit card","Cash","No charge","Dispute"))
df$RateCodeID<-factor(df$RateCodeID,labels=c("Standard","JFK","Newark","Nassau/Westchester","Negotiated"))


#RECORDAR CANVIAR NA's A Payment_type per "No charge" (INVESTIGAR COM)