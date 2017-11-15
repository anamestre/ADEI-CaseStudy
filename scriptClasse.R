# Sessió 1 amb la Lídia :)


attach(anscombe) # MAI FER SERVIR EN EXÀMENS NI SITUACIONS DELICADES!!!!
                 # S'haurà de fer un dettach després
                 # Per estalviar-nos posar coses com: anscombe$nomVariable
XA
plot(XA, YA, main="Joc de dades A", pch=19)
ma <- lm(YA~XA, data=anscombe) # Per defecte asumeix que és una constant
summary(ma)
# Suposar normalitat ens proporciona un interval de confiança per a l'estimació