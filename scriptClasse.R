# Sessió 1 amb la Lídia :)


# ####################### DATASET A #######################
# #########################################################

attach(anscombe) # MAI FER SERVIR EN EXÀMENS NI SITUACIONS DELICADES!!!!
                 # S'haurà de fer un dettach després
                 # Per estalviar-nos posar coses com: anscombe$nomVariable
XA
plot(XA, YA, main="Joc de dades A", pch=19)
ma <- lm(YA~XA, data=anscombe) # Per defecte asumeix que és una constant
summary(ma)
# Suposar normalitat ens proporciona un interval de confiança per a l'estimació
# Y = 3 + 0.5X + Epsilon    ; Epsilon = residus   ;Y = B1 + B2X + Epsilon
# Y és el target, que volem explicar a partir d'una variable numérica, la X.

# Comentaris Summary:
# Multiple-R -> el meu model sistematic explica el 66'65% de variabilitat del meu target. 

cbind(XA, fitted.values(ma)) #fitted.value -> predicció del model
lines(XA, fitted.values(ma), lwd=2, lty=2)
text(XA, YA, row.names(anscombe)) # add es per adjustar la visibilitat

# Anàlisi dels residus
par(mfrow=c(2,2)) # diagnosi de la talla
plot(ma, id.n = 5) #id.n = quants volem identificar, per defecte R ens identifica 3
                   # pero no volen dir res



# ####################### DATASET B #######################
# #########################################################

XB
par(mfrow=c(1,1))
plot(XB, YB, main="Joc de dades B", col = "red",pch=19)
mb <- lm(YB~XB, data=anscombe) # Per defecte asumeix que és una constant
summary(mb)

cbind(XB, fitted.values(mb)) #fitted.value -> predicció del model
lines(XB, col = "red", fitted.values(mb), lwd=2, lty=2)
text(XB, YB, col = "red", row.names(anscombe)) # add es per adjustar la visibilitat

# Anàlisi dels residus
par(mfrow=c(2,2))   # diagnosi de la talla
plot(mb, id.n = 5)  #id.n = quants volem identificar, per defecte R ens identifica 3
                    # pero no volen dir res

# Resolució
mb2 <- lm(YB~XB+I(XB^2), data=anscombe) # Per defecte asumeix que és una constant
summary(mb2)
  # Què veiem?
  # component sistemàtica que hem estimat (mu = B1 + B2X): -6 + 2.7X - 0.13X^2




# ####################### DATASET C #######################
# #########################################################

XC
par(mfrow=c(1,1))
plot(XC, YC, main="Joc de dades B", col = "green",pch=19)
mc <- lm(YC~XC, data=anscombe) # Per defecte asumeix que és una constant
summary(mc)

cbind(XC, fitted.values(mc)) #fitted.value -> predicció del model
lines(XC, col = "green", fitted.values(mc), lwd=2, lty=2)
text(XC, YC, col = "green", row.names(anscombe)) # add es per adjustar la visibilitat

# Anàlisi dels residus
par(mfrow=c(2,2))   # diagnosi de la talla
plot(mc, id.n = 5)  #id.n = quants volem identificar, per defecte R ens identifica 3
                    # pero no volen dir res


# Outlier residus
library(car)
par(mfrow=c(1,1))
Boxplot(resid(mc))

# Indicator: Cook's distance of linear model
cooks.distance(mc) # Com més alta és la distància de cook a una observació, vol dir que és important.
Boxplot(cooks.distance(mc), main="Distancia Cook")

# Solution
mc2 <- lm(YC~XC, data=anscombe[-3,]) # no 3
summary(mc2)
plot(XC, YC, col="green", pch=19)
lines(XC, fitted.values(mc), lwd = 2, lty=2, col="green" )
lines(anscombe$XA[-3], fitted.values(mc2), col="darkgreen", lwd=3, lty=3)


# ####################### DATASET D #######################
# #########################################################

XD
par(mfrow=c(1,1))
plot(XD, YD, main="Joc de dades B", col = "blue",pch=19)
md <- lm(YD~XD, data=anscombe) # Per defecte asumeix que és una constant
summary(md)

cbind(XD, fitted.values(md)) #fitted.value -> predicció del model
lines(XD, col = "blue", fitted.values(md), lwd=2, lty=2)
text(XD, YD, col = "blue", row.names(anscombe)) # add es per adjustar la visibilitat

# Anàlisi dels residus
par(mfrow=c(2,2))   # diagnosi de la talla
plot(md, id.n = 5)  #id.n = quants volem identificar, per defecte R ens identifica 3
# pero no volen dir res
# Ens hem de mirar bé sempre els Warnings

# Influet data?
cooks.distance(md) # Com més gran és, més influent és
hatvalues(md)

# Solution
md2 <- lm(YD~XD, data=anscombe[-3,]) # no 3
summary(md2)
abline(h=mean(YD), col="blue", lty=4)