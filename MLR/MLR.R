setwd("C:\Users\varda\Desktop\DAA Tutorial\Advertisement_MLR")
survey<- read.table('Advertisement_MLR.csv',header=TRUE,sep=',')

x1bar<-(sum(survey$budgetTv))/6
x2bar<-(sum(survey$budgetNews))/6
ybar<-(sum(survey$totalUnits))/6

findComp1<-function(x1)
  {
    return(x1-x1bar)
  }

findComp2<-function(x2)
  {
    return(x2-x2bar)
  }

findComp3<-function(y)
  {
    return(y-ybar)
  }

findComp4<-function(x1)
  {
    return((x1-x1bar)*(x1-x1bar))
  }

findComp5<-function(x2)
  {
    return((x2-x2bar)*(x2-x2bar))
  }

n1<- sapply(survey$budgetTv,findComp1)
n1

n2<-sapply(survey$budgetNews,findComp2)
n2

n3<- sapply(survey$totalUnits,findComp3)
n3


#finding beta-1

n4<- n1*n3
n4

numerator<-sum(n4)
numerator

den<-sapply(survey$budgetTv,findComp4)
den

deno<-sum(den)
deno

beta1<- numerator/deno
beta1


#finding beta-2

n5<- n2*n3
n5

numerator<-sum(n5)
numerator

den<-sapply(survey$budgetNews,findComp5)
den

deno<-sum(den)
deno

beta2<- numerator/deno
beta2

#beta-0

beta0<- ybar-(beta1*x1bar)-(beta2*x2bar)
beta0

pred1<-beta0+(beta1*5000)+(beta2*150)
pred1
pred2<-beta0+(beta1*6000)+(beta2*300)
pred2
pred3<-beta0+(beta1*7000)+(beta2*450)
pred3


predicted<-c(pred1,pred2,pred3)
predicted

predictor<-c(5000,6000,7000)
predictor<-c(150,300,450)
plot(predictor,predicted)


#using inbuilt function for mulitple linear regression
model <- lm(totalUnits ~ budgetTv+ budgetNews,data=survey)
summary(model)

