
# SCRIPT SUPER MÀGIC PER APROVAR EL QUIZ2 DE ADEI.

# PRÈVIAMENT: Carregar les dades:
library(FactoMineR)
library(car)
library(missMDA)
library(ggplot2)
library(factoextra)
library(mvoutlier)
library(RColorBrewer)
setwd("Path/Al/Fitxer")
load("TRB18DOE-raw.RData")

# Coses de teoria:

P-value <= 0.05: The data do not follow a normal distribution (Reject the Null Hypothesis)


# Tipus de preguntes + tipus de resolució :) 

# QUIZ2 ADEI CURS 16-17Q2
# #######################

# 1. Summarize numerically and graphically the response variable. + Interpretation
#    Do you think that fuelc may be considered normally distributed?
	 summary(kpinet$fuelc)
	 Boxplot(kpinet$fuelc,col="cyan")
	 
	50% of samples lie between 14590 and 17470 liters, both Q1 and Q3 quantiles. 
	Mean and median are not close confirming an asymmetric distribution.
	No outliers are present since min and max values are not extreme values. 
	 
	hist(kpinet$fuelc,freq=F,10,col=rainbow(10))
	mm<-mean(kpinet$fuelc);dd<-sd(kpinet$fuelc);mm;dd
	curve(dnorm(x,mm,dd),add=T,col="red",lwd=2)
	lines(density(kpinet$fuelc),add=T,col="grey70",lwd=2,lty=2)
	 
	shapiro.test(kpinet$fuelc)
	Shapiro Test rejects the null hypothesis of normality. (P-value)
	 
	 
# 2. Which are the variables statistically associated with the target (fuelc)? 
# Indicate the suitable measure of association and/or tests that support your answer.

	varkpis<-names(kpinet)[c(1:6,8:17,25)] # QUINES VARIABLES S'HAN DE POSAR AQUÍ????!!!!!
	condes(kpinet[,varkpis],which("fuelc"==colnames(kpinet[,varkpis])))
	
	The question has to be answered using profiling procedure: condes().
	
	$quanti
	Numeric variables that are global and positively related to the target (fuelc):
	 - Associades: Són les primeres que surten del output de condes i són positives. 
	 - Inversament relacionades: les que tenen una correlation negativa. 
	
	$quali
	Factors: Factor variables that are globally associated to fuelc from high to low significance are: guidance penetration
	(GP), demand pattern (DP), driver configuration (TD) and time-window interval (TW) factor.
	
	$category
	Levels of factors that show a positive additive effect over the grand mean of fuelc are: guidance penetrations 
	80, 90 and 100; time window 1.5; demand pattern 30% and driver configuration 60-20-20. On the negative side:
	guidance penetrations 0, 10 and 50; time window 3 min, driver configuration TD-40-50-10 (basic scenario)
	and demand pattern 0% (basic scenario).
	
	

# 3. The average fuelc can be argued to be the same for all Demand Pattern levels (DP)? Which are the groups
# that show a significant greater average fuelc than the others?.

	Boxplot(kpinet$fuelc~kpinet$DP,col=rainbow(4))
	kruskal.test(kpinet$fuelc~kpinet$DP)
	
	According to non parametric Kruskal-Wallis test on means of fuelc according to demand levels, the null
	hypothesis of uniform mean can be clearly rejected.It is shown in a boxplot for fuelc according to levels on
	demand pattern (DP), and as demand increases (number of trips), the total fuel consumption also increases.
	
	Es comenta el boxplot, i fent el test de Kruskal-Wallis veiem que p-value < 0.05 :D 

# 4. Let us perform a one-way analysis of variance on TRB2018 dataset for target fuelc on Demand Pattern
# (DP) factor using the standard general linear model (lm() method). Assess the explicability of the model and
# interpret the model for prediction purposes writing the equation for each GP level.

	m1<-lm(fuelc~DP,data=kpinet)
	summary(m1)
	
	The model containing the gross effect of Demand Pattern (quantity of trips) explains 55% of the total
	variability of the fuel consumptation. -> Mirem al summary el Multiple R-squared.
	
	Consumption of fuel (liters) at the base demand is predicted as 13579.5 liters, -> Mirant el primer valor del summary,
	fila de (Intercept)
	
	as demand is increased in long distance trips by 10, 20 and 30% -> Les següents files

	the average consumption is predicted as (13579.5+1905.5=15485.0)liters, 
	(13579.5+3125.4=16704.9) liters and (13579.5 +4338.6=17918.1) liters, respectively.
	
	tapply(kpinet$fuelc,kpinet$DP,mean)
	Ens serveix per confirmar les sumes
	
# 5. Use an inferential method to quantify the probability of the null hypothesis of a negligible gross effect for
# demand pattern (DP) factor.

	m0<-lm(fuelc~1,data=kpinet) #QUÈ VOL DIR AQUEST 1???!
	m1<-lm(fuelc~DP,data=kpinet)
	anova(m0,m1)
	
	Clearly, a Fishert test is suitable to test to calculate pvalue for the hypothesis. P value is very small (Fisher
	statistic is 63.9, very large) thus rejecting the null hypothesis: Demand Pattern (number of trips) significantly
	affects the total fuel consumption of the network.
	
	
# 6. Let us perform a two-way analysis of variance on TRB2018 dataset for target fuelc on Guidance Penetration
# (GP) and Demand Pattern (DP) factors using the standard general linear model (lm() method). Are
# interactions needed for GP and DP to explain the total fuel consumption in the network? Are net effects for
# both factors significant?

	m2<-lm(fuelc~GP+DP,data=kpinet)
	m3<-lm(fuelc~GP*DP,data=kpinet)
	anova(m2,m3)
	
	# AQUESTA RESPOSTA NO M'HA QUEDAT MASSA CLARA... :( 
	
	According to Anova(m2) output, the net effects for both factors are significant.  -> Pvalue <= 0.05
	The complete two-way anova 	model can’t be calculated since many NA estimators appear. 
	Anyway, since additive GP+DP and complete GP*DP models are nested, then a Fisher test can be used to check
	the need for interactions: since p value is less than 0.05 regular threshold, both model are not equivalent, 
	thus interactions (the few that can be estimated) contribute to explain total fuel consumption.
	
# 7. Assess the explicability of the BEST model selected in question 6 and calculate the point and 95% confidence
# prediction interval for fuel consumption for an scenario with a guidance penetration of 80% and demand level
# of 30%. Try to explain the reason of warning messages appearing for some methods depending on the selected mode.
	
	predict(m3,newdata=data.frame(GP="GP-80",DP="DP-30"))
	predict(m2,newdata=data.frame(GP="GP-80",DP="DP-30"))
	
	Miro quin dóna més gran, en aquest cas triem m3.
	
	predict(m3,newdata=data.frame(GP="GP-80",DP="DP-30"),interval="prediction")
	
	The experimental design does not allow to estimate interactions between GP and DP factors, thus many of
	the dummy variables can not be determined. For the particular levels of 80% GP and 30% DP the point
	prediction is: 13145.04+1490.20 +2529.35+ 1964.15=19128.74 liters. 
	-> Mirant el summary(m3):
		Valor intercept + GPGP-80 + DPDP-30 + GPGP-80:DPDP-30
	
	The 95% confidence interval for total consumption is [18361.75 19895.72], in liters.
	-> Mirant el predict llarg, agafant els valors lower i upper
	
	
# 8. Consider a general linear modeling for total fuel consumption when all numeric variables included in the
# list of KPIs are taken into account. Assess explicability, try to reduce the number of explanatory variables
# without loosing too much explicability and discuss colinearity among explanatory variables.

	m10<-lm(fuelc~co2+nox+ttt.h+ttdis+mflow+density+mtt.s.km+mdelay.s.km+mspeed, data=kpinet)
		Posem les variables quantis del principi. (Pregunta 2)
	summary(m10)

	Explanatory variables are correlated between them. The explicability according to the coefficient of determi-
	nation is 99.93% (del summary Multiple R-squared:), so it explains 99.93% of the total variability in fuelc, 
	but the model is not suitable as can
	be seen using vif(), variance inflation factor indicators are over 10 (larger) and step(m10) method does not
	help us to define a proper model, since no variable is removed. A proposal containing 2-3 non correlated
	variables should be proposed: no automatic procedure support the decision process.
	
	vif(m10)
	step(m10)
	names(kpinet)
	round(cor(kpinet[,c(15,16,17,10,14,9,8,11,12,13)]),dig=2)
	
	
# 9. Consider linear models including as explanatory numeric variables total travel distance (ttdis) and mean
# speed (mspeed) and the main effect for guidance penetration factor. Discuss the model, while justifying the
# choice (nor residuals or influent data analysis has to be considered). 	
	-> Nor residuals or influent data analysis has to be considered... --> aplicar step??
	
	m20<-lm(fuelc~ttdis+mspeed+GP, data=kpinet)
	step(m20)
	Anova(m20)
	summary(m20)
	vif(m20)
	
	Specified model explains 99.5% of the variability of fuel consumption (summary), thus is apparently an excellent model.
	All net effects are significant and the model can not be reduced by step() method.  
		(si hi apliquem step, no treu res)
	
	Variance inflation factors are less than 3, so no colinearity problems are present. -> vif(m20)
	
	
# 10. Consider the model for fuel consumption proposed in the previous question. Does the model fullfil the
# properties for linear models? Justify your answer using statistical arguments.Indicate the presence of lack of
# fit observations and influent data.
	
	par(mfrow=c(2,2))
	plot(m20)
	
	par(mfrow=c(1,1))
	residualPlots(m20,id.n=5,id.method=cooks.distance(m20),col="grey70",pch=19)
	
	marginalModelPlots(m20,id.n=5,id.method=cooks.distance(m20),col="grey70",pch=19)
	
	influencePlot(m20,id.n=5)
	
	Boxplot(rstudent(m20),col="orange") # Lack of fit: outlier in residuals
	
	kpinet[134,]
	
	Boxplot(cooks.distance(m20),col="red")
	
	abline(h=4/(nrow(kpinet)-length(coef(m20))),col="red",lwd=2,lty=2)
	#Chatterji-Hadi cut-off 4/(n-p) is suitable due to small sample size
	
	# Mirar gràfiques al pdf
	Normality seems to be met, as homocedasticity, but a systematic pattern seems to be present in residuals
	(upper-left display) indicating the need of a non-linear transformation or the addition of a new variable (as it
	can be seen using the MarginalModelPlots(m20), profiles for low speeds configurations indicates two groups of
	observations). Lack of fit is present in observation 134 (student residual > 3). Chatterji-Hadi cut-off 4/(n-p)
	for Cooks distance is suitable due to small sample size: observation 134 is the most influent, followed by 3, 1,
	5 and 120.