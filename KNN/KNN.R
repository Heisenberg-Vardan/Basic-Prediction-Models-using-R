fin=file.choose()
f=read.csv(fin)
f

age<- readline(prompt="Enter age:")
age<- as.integer(age)

loan<- readline(prompt="Enter loan amount:")
loan<- as.integer(loan)

age
loan

d=vector()
length(d)=nrow(f)

for(i in 1:nrow(f))
  {
    d[i]=sqrt(((age-f[i,1])^2)+((loan-f[i,2])^2))
  }
d

min(d)

neighbour<-which.min(d)
neighbour

print(paste("Customer Age :", age,", Loan Taken :",loan,", Expected Payment : ",f[neighbour,2]))

