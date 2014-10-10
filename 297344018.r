
#I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm<-function(n,mean=0,sd=1){
  #Purpose:produce a vector of random normal deviates 
  #Input:n numeric vectors of random uniform deviates
  #Output:A numeric vector of length n, each element a random (standard unless otherwise specified using mean and sd arguments) normal deviate
  
  if (n<0|sd<0){
    stop("invalid arguments")
  } 
  
  y<-rep(0, times=n) 
  for (i in 0:n){
    #transform input to an N(0,1) random deviate using CLT (Rubinstein 1981:89-90)
    x<-((sum(runif(16))-8)*((12/16)^(1/2)))
    #adjust mean and standard deviation as required
    y[i]<-((x*sd)+mean) 
  }
  
  return(y)   
}

my.rchisq<-function(n,df=1){
  #Purpose:Produce a vector of random chi squared distributed deviates
  #Input:n numeric vectors, length df, of random normal deviates with df=1 (unless otherwise specified)
  #Output:A numeric vector, length n, each element a  chi squared distributed random deviate
 
  if (n<0|df<0){
    stop("invalid arguments")
  }
  
  y<-rep(0, times=n)
  for (i in 0:n){
    #transform normal deviates produced my my.rnorm() to a chi squared deviate
    x<-sum((my.rnorm(df))^2)
    y[i]<-x
  }
  return(y)
}

my.rf<-function(n,df1=1,df2=1){
  #Purpose:Produce a vector of random numbers with an F distribution
  #Input:A numeric vector of 2 Chi Square distributed deviates
  #Output:A numeric vector, length n, each element an F distributed random number
  
  if (n<0|df1<0|df2<0){
    stop("invalid arguments")
  }
  y<-rep(0, times=n)
  for (i in 0:n){
    #create vector x, length 2, each element is chi squared distributed with degrees of freedom specified by df1 &df2 
    x<-c(my.rchisq(1,df1),my.rchisq(1,df2))
    #transform random chi squared distributed deviates to F distributed numbers
    y[i]<-((x[1]/df1)/(x[2]/df2))
    
  }
  return(y)
}

test.my.rnorm<-function(n,mean=0,sd=1){
  #Purpose:test whether my.rnorm returns normally distributed deviates
  #Input: vector of deviates created by function my.rnorm
  #Output:Graphical representation of data and result of a Shapiro-Wilk test at a 95% confidence level
  
  #Set output device to show 2 plots on one page
  par(mfrow=c(1,2))
  
  # do a qq plot with light blue x=y line,  and a histogram with normal distribution overlayed in magenta and density curve in blue
  # assistance in overlaying normal distribution found here;(http://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r)
  b<-(my.rnorm(n,mean,sd))
  qqnorm(b)
  abline(0,1, col="aquamarine1",lwd=1)
  hist(b, prob=TRUE, xlab="Random deviates produced by my.rnorm(n,mean,sd)", main="Histogram of my.rnorm with Normal Distribution")
  curve(dnorm(x,mean,sd),col="magenta",lwd=2, yaxt="n",add = TRUE)
  lines(density(b),col="blue",lwd=2, yaxt="n" )
  
  #set output device to show 1 plot on page
  par(mfrow=c(1,1))
  
  #for Shapiro-Wilk test n cannot exceed 5000, if n does, present error message 
  if(n>5000){
    stop("shapiro-wilk test has max-n=5000,see qq & hist")
  }
  
  #perform Shapiro-Wilk test on my.rnorm(n), save pvalue as a
  #if a>0.05 we do not reject the hypothesis that my.rnorm(n) are from a normal distribution
  a<-shapiro.test(my.rnorm(n,mean,sd))$p.value
  pass.test<-(a>=0.05)
  return(cat("Shapiro-Wilk test produced a p value of",a,"- deviates produced by my.rnorm() from a normal distribution;",pass.test,",see qq plot and histogram\n"))
  
}

test.my.rchisq<-function(n,df=1){
  #Purpose:test whether my.rchisq returns chi square distributed deviates
  #Input: vector of deviates created by function my.rchisq
  #Output:result of a Kolomogorov-Smirnov (K-S) test at a 95% confidence level
  
  #perform K-S test,compare data from my.rchisq and rchisq, if p value is >0.05 cannot reject null hypothesis that data is from same distribution (chi squared) 
  a<-ks.test(my.rchisq(n,df),rchisq(n,df))$p.value
  pass.test<-(a>=0.05)
  return(cat("Kolmogorov-Smirnov test produced a p value of",a,"- deviates produced by my.rchisq() are from a chi squared distribution;",pass.test,"\n"))
  
}

test.my.rf<-function(n,df1=1,df2=1){
  #Purpose:test whether my.rf returns F distributed deviates
  #Input: vector of deviates created by function my.rf
  #Output:result of a Kolomogorov-Smirnov (K-S) test at a 95% confidence level
  
  #perform K-S test,compare data from my.rf and rf, if p value is >0.05 cannot reject null hypothesis that data is from same distribution (F Distribution) 
  a<-ks.test(my.rf(n,df1,df2),rf(n,df1,df2))$p.value
  pass.test<-(a>=0.05)
  return(cat("Kolmogorov-Smirnov test produced a p value of",a,"- deviates produced by my.rf() are from an F distribution;",pass.test,"\n"))
  
}



#Rubinstein, R.Y.1981. Simulation and the Monty Carlo method. John Wiley and Sons.