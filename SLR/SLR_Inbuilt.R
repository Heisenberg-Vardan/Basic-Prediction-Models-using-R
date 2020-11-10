setwd("C:\Users\varda\Desktop\DAA Tutorial\Advertisement_SLR")

survey<- read.table('Advertisement_SLR.csv',header=TRUE,sep=',')
head(survey)

linearMod<-lm(totalUnits~budget,data=survey)
print(linearMod)

modelSummary<-summary(linearMod)
modelCoeffs<-modelSummary$coefficients
modelCoeffs
