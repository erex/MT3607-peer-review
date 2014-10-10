# I confirm that the attached is my own work, except where clearly indicated in the text.
my.rnorm<-function(n,mean=0,sd=1,print=T){ 
  if (round(n)!=n| n<=0| sd<0)
    stop("invalid arguments\n")
  #test error 
  b<-numeric(n)
for (i in 1:n){
  a<- runif(16)
b[i]<-sd*(sum(a)-8)*(12/16)^(0.5)+mean
}
# create the norm
if(print==T){print(b)}
else{b}
# make sure the print code do not work in the my.chisq
}

my.rchisq<-function(n,df=1,print=T){
  c<-numeric(n)
  if (round(n)!=n|n<=0|round(df)!=df|df<=0){
    cat("invalid arguments\n")
  }
  #test error
  else{
    for (i in 1:n){
      c[i]<-sum(my.rnorm(df,0,1,print=F)^2)
    }
    if(print==T){print(c)}
    else{c}
    # make sure the print code do not work in the my.rf
  }
}


my.rf<-function(n,df1=1,df2=1){
  #test error again
  if (round(n)!=n| n<=0| round(df1)!=df1|df1<=0|round(df2)!=df2|df2<=0)
    stop("invalid arguments")
  f<-seq(n)
  for (i in 1:n){
    f[i]<-(my.rchisq(1,df1,print=F)/df1)/(my.rchisq(1,df2,print=F)/df2)
}
print(f)
}

#automated test
# test my.norm
x<- my.rnorm(100,23,2)
p<-shapiro.test(x)$p.value
# calculate the probability of normal distribution by shapiro test 
pass.test<-(p>0.05 & length(x)==100 & is.numeric(x))

# test my.rchisq
x<- my.rchisq(1000,5)
a<-qchisq(0.05,5)
# get the value of the chisq (in same df) with 95% probability
b<-0
for (i in 1:1000){
  if (x[i]>a) b<-b+1}
#calculate the times when the value of the chisq larger than the value we get 
c<-pbinom(b,1000,0.95)
#calculate the probability of this case
pass.test<-(c>0.05 & length(x)==1000 & is.numeric(x))

