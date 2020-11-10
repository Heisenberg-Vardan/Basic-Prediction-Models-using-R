setwd("C:\Users\varda\Desktop\DAA Tutorial\Advertisement_SLR")
survey<- read.table('Advertisement_SLR.csv',header=TRUE,sep=',')

xbar<-(sum(survey$budget))/6
ybar<-(sum(survey$totalUnits))/6

findComp1<-function(x)
  {
    return(x-xbar)
  }

findComp2<-function(y)
  {
    return(y-ybar)
  }

findComp3<-function(x)
  {
    return((x-xbar)*(x-xbar))
  }

n1<- sapply(survey$budget,findComp1)
n1

n2<- sapply(survey$totalUnits,findComp2)
n2

n3<- n1*n2
n3

numerator<-sum(n3)
numerator

den<-sapply(survey$budget,findComp3)
den

deno<-sum(den)
deno

beta1<- numerator/deno
beta1

beta0<- ybar-(beta1*xbar)
beta0

pred1<-beta0+(beta1*5000)
pred1

pred2<-beta0+(beta1*6000)
pred2

pred3<-beta0+(beta1*7000)
pred3

predicted<-c(pred1,pred2,pred3)
predicted

predictor<-c(5000,6000,7000)
plot(predictor,predicted)
