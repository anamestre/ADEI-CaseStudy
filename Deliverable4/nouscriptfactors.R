---
  title: "Untitled"
output: html_document
---
  
  ## Load Required Packages
  
  ```{r, echo=FALSE, include=FALSE}
rm(list=ls())
# Load Required Packages: to be increased over the course

requiredPackages <- c("effects","FactoMineR","car", "factoextra","RColorBrewer","ggplot2","mvoutlier")
missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]

if(length(missingPackages)) install.packages(missingPackages)
lapply(requiredPackages, require, character.only = TRUE)

```
# Statistical Modelling

## Load Data after Cleaning and EDA

```{r}
# green_tripdata_2016-01
load("MyTaxi5000Clean.RData")
summary(dfD4)
names(dfD4)
```

## Multiple Linear Regression issues

*Target numeric Total_amount*
  
  ```{r}
names(dfD4)
vars_con<-names(dfD4)[c(3:15,17:21)];vars_con
vars_dis<-names(dfD4)[c(1,2,16,22:36)]
vars_res<-names(dfD4)[c(15,23)]
vars_cexp<-vars_con[c(1:12,14:18)];vars_cexp

```

# Using factors as explanatory variables

```{r}
# I adapt the best model obtained in last class
m241<-lm(log(Total_amount)~tlenkm+Extra+MTA_tax+Tip_amount+Tolls_amount,data=dfD4)
summary(m241)

# boxcox: Transform target to improve linear properties
# Consider polynomial transformations of covariates (numeric explanatory variables)

# Fent els deures ...

m27<-lm(log(Total_amount)~poly(tlenkm,3)+Extra+MTA_tax+poly(Tip_amount,2)+Tolls_amount,data=dfD4)

anova(m241,m27)
summary(m27)
Anova(m27)
vif(m27)
plot(allEffects(m27))
```

# Check if any covariate can be substituted by a factor

```{r}
# Consider to deal with some of the current variables as factors: Extra and MTA_tax

#(peak morning 7-9, off-peak 10-16, peak afternoon 17-20, night 21-6)

assignPeriod <- function(pickup) {
  pickupN <- as.numeric(pickup)
  result <- "None"
  if (pickupN < 8) { result <- "Night"}
  else if (pickupN < 11) {result <- "MorningPeak"}
  else if (pickupN < 18) {result <- "OffPeak"}
  else if (pickupN < 22) result <- {"AfternoonPeak"}
  else { result <- "Night"}
  return(result)
}

for (i in 1:5000) {
  dfProva[i,]$period <- assignPeriod(dfProva[i,]$pickup) 
}

dfD4$period <- as.factor(dfProva$period)

table(dfD4$Extra)
table(dfD4$MTA_tax)
dfD4$f.extra<-0
dfD4$f.extra[dfD4$Extra>0]<-1
dfD4$f.extra[dfD4$Extra>0.5]<-2
dfD4$f.extra<-factor(dfD4$f.extra,labels=c("Extra-No","Extra-0.5","Extra-1"))
dfD4$f.MTA<-ifelse(dfD4$MTA_tax>0,1,0)
dfD4$f.MTA<-factor(dfD4$f.MTA,labels=c("TAX-No","TAX-Yes"))

m27<-lm(log(Total_amount)~poly(tlenkm,3)+Extra+MTA_tax+poly(Tip_amount,2)+Tolls_amount,data=dfD4)

m28<-lm(log(Total_amount)~poly(tlenkm,3)+f.extra+f.MTA+poly(Tip_amount,2)+Tolls_amount,data=dfD4)

summary(m28)
# Arguments: R2 and BIC. Fisher Test IS NOT possible!!!
BIC(m27,m28)
# Millor: m27 o m28
```

# NEW Factors in the predictor
```{r}
names(dfD4)
vars_dis<-names(dfD4)[c(1,4,5,19:22,25:29)];vars_dis
vars_dis  # Totes les discretes

condes(dfD4[,c("Total_amount",vars_dis)],1)  # Choose as candidate vars those more related to target

names(dfD4)
# For practical reasons in class m27 instead of m28 (The best). I adapt to your file status!!!

m37<-lm(log(Total_amount)~poly(tlenkm,3)+Extra+MTA_tax+poly(Tip_amount,2)+Tolls_amount+RateCodeID+Payment_type+f.hour+f.passcount+f.impro+VendorID,data=dfD4)
summary(m37)
m38<-lm(log(Total_amount)~poly(tlenkm,3)+Extra+MTA_tax+poly(Tip_amount,2)+Tolls_amount+RateCodeID+Payment_type+period+f.passcount+f.impro+VendorID,data=dfD4)
summary(m38)
BIC(m37,m38)

summary(m37)

anova(m27,m37)  # Milloro moderadament Fisher Test
Anova(m37)
Anova(m28, m38)

m39<-step(m37,k=log(nrow(dfD4)))  # I should use m38 (factor period, no hour)
m39<-step(m38,k=log(nrow(dfD4)))  # I should use m38 (factor period, no hour)

Anova(m39)
vif(m39)  # Problems: remove one var
plot(allEffects(m39))
```

## Adding interactions to factors and covariates
```{r}
# Best model obtained
m50<-lm(log(Total_amount) ~ poly(tlenkm, 3) +     poly(Tip_amount, 2)+ Extra + Tolls_amount + RateCodeID + Payment_type + period, data = dfD4)

m51<-lm(log(Total_amount) ~ ( poly(tlenkm, 3) +     poly(Tip_amount, 2)+ Extra + Tolls_amount)*period + (RateCodeID + Payment_type )* period, data = dfD4)

m52<-step(m51,k=log(nrow(dfD4)))
summary(m52)

Anova(m52)
vif(m52)

plot(allEffects(m52))

### Model Validation 
par(mfrow=c(2,2))
plot(m52,id.n=8)
par(mfrow=c(1,1))

llista<-influencePlot(m52,id.n=10);length(llista);llista

# Remove outliers and influent data
llrem<-which(row.names(dfD4)%in%row.names(llista));llrem
dfD41<-dfD4[-llrem,]

m61<-lm(log(Total_amount) ~ ( poly(tlenkm, 3) +     poly(Tip_amount, 2)+ Extra + Tolls_amount)*period + (RateCodeID + Payment_type )* period, data = dfD41)

m62<-step(m51,k=log(nrow(dfD41)))
summary(m62)

par(mfrow=c(2,2))
plot(m62,id.n=8)
par(mfrow=c(1,1))

# Iterate ...

```

