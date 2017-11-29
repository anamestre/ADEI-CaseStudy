m24<-lm(Total_amount-Trip_distance+Extra+MTA_tax+Tip_amount+Tolls_amount, data=df4[-ll,])
summary(m24)

names(df4)
ll<-which(is.na(df4$Trip_distance));ll
#Transform vars: polinomic
m25<-lm(formula = Total_amount - poly(Trip_distance,2) + Extra + MTA_tax + Tip_amount + Tolls_amount, data = dfSenseNA)

#to interpret (per modelar no va bé)
m25iii <- lm(Total_amount-Trip_distance+I(Trip_distance^2) + Extra+MTA_tax+Tip_amount+Tolls_amount, data=df4[-ll,])
summary(m25iii)
#avantatges: treballar amb net effects
library(car)
Anova(m25)

#per veure efectes marginals:
library(effects)
plot(allEffects(m25))

#othertransformations:
marginalModelPlots(m25)



dfSenseNA <- df4
ll<-which(is.na(df4$Tolls_amount));ll
dfSenseNA <- dfSenseNA[-ll,]

m26<-lm(formula = log(Total_amount) - poly(Trip_distance,2) + Extra + MTA_tax + Tip_amount + Tolls_amount, data = dfSenseNA)
summary(m26)
Anova(m26)

#valorar heterocedasticitat:
library(lmtest)
bp.test(m26)

#detecció dels outliers dels residus
qqq <-summary(rstudent(m26))
sup <- qqq[5]+3*(qqq[5]-qqq[2])

boxplot(rstudent(m26))
abline(h=sup, col="red", lwd=2)
abline(h=-sup, col="red", lwd=2)

#Relaxed criteria abot severe outlier, taken into account sample size: abs(rsdtudent(m26)) < 5

llout = which(abs(rstudent(m26)) < 5); length(llout) #residual outliers
rstudent(m26)[llout]

#a priori influent data: far from center of gravity of explanatory variables

quantile(hatvalues(m26), seq(0.1, 0.1))
#cutoff ins 2*p/n (small samples) 5*p/n
cuthat <- 5*length(coef(m26))/nrow(df); cuthat
boxplot(hatvalues(m26))
abline(h=cuthat, col="blue", lwd=2, lty=2)
#cutoff relaxed to 0.02
llev <- which(hatvalues(m20) > 0.02)

#○who is who
df[llev, vars_con] #¿?

#influent data (a posteriori): Cook's distance
boxplot(cooks.distance(m26))
abline(h = 0.05, col="green")
llcook <- which(cooks.distance(m26) > 0.05)
df[llcook, vars_con]

influenceIndexPlot(m26)
influencePlot(m26, col="orange", id.n=10)

dfbetas(m26)

matplot(dfbetas(26), type="l")
nn <- dim(df)[1]
abline(h = sqrt(4/nn), lty=3, col=6)
abline(h = -sqrt(4/nn), lty=3, col=6)
#legend here