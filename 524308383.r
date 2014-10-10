#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){
#Purpose: Random normal deviates generator function. Uses the uniform random number
#generator in R to generate random normal deviates.
#Inputs:
# n - number of values to return
# mean - mean of the values to return - default 0
# sd - standard deviation of the values to return - default 1
#Outputs:
# a vector of pseudo-random values from a normal distribution
  
  #traps input errors and stops function execution if an error is found
  if ((missing(n)==TRUE)||(length(n)>1)||(is.numeric(n)==FALSE)||(is.numeric(mean)==FALSE)||(is.numeric(sd)==FALSE)||(n%%1!=0)||(n<=0)||(length(mean)>1)||(length(sd)>1)||(sd<0))
    stop("invalid arguments")
  
  #creates numeric vectors: one of length 16 called y & one of length n called x
  y<-numeric(16)
  x<-numeric(n)
  
  #for i=1,...,n excutes the code contained within {}
  for (i in 1:n){
    
    #for j=1,..,16 generate a random uniform deviate with mean=0 and sd=1 and   
    #store this in element j of vector y 
    for (j in 1:16){
      y[j]<-c(runif(1))
    }
    
    #calculates the ith random normal deviate & transforms it into a value with the 
    #input mean & sd & then stores it in element i of vector x
    x[i]<-((((sum(y))-8)*sqrt(12/16))*sd) +mean
  }
  
  #returns the vector of pseudo-random values from a normal distribution
  return(x) 
}


my.rchisq<-function(n,df=1){
#Purpose: Chi-squared distributed deviates generator function.  Creates chi-squared
#distributed deviates from random normal deviates.
#Inputs:
# n - number of values to return
# df - degrees of freedom of the distribution - default 1
#Outputs:
# a vector of pseudo-random chi-squared distributed deviates
  
  #traps input errors and stops function execution if an error is found
  if ((missing(n)==TRUE)||(length(n)>1)||(is.numeric(n)==FALSE)||(n%%1!=0)||(n<=0)||(is.numeric(df)==FALSE)||(df%%1!=0)||(length(df)>1)||(df<0))
    stop("invalid arguments")
  
  #creates a numeric vector of length n called chi
  chi<-numeric(n)
  
  #for i=1,...,n obtains df random normal deviates by calling my.rnorm; squares  
  #their values and assigns these to vector x; calculates the ith random chi-
  #squared-distributed deviate by summing x & stores this in element i of vector chi
  for (i in 1:n){
    x<-(my.rnorm(df))^2
    chi[i]<-sum(x)
  } 
  
  #returns the vector of pseudo-random chi-squared distributed deviates
  return(chi)
}
  

my.rf<-function(n,df1=1,df2=1){
#Purpose: Random F-distributed deviates generator function.  Creates random numbers
#with an F-distribution from chi-squared distributed deviates.
#Inputs:
# n - number of values to return - no default
# df1 - degrees of freedom of the numerator - default 1
# df2 - degrees of freedom of the denominator - default 1
#Outputs:
# a vector of pseudo-random F-distributed deviates
  
  #traps input errors and stops function execution if an error is found
  if ((missing(n)==TRUE)||(length(n)>1)||(is.numeric(n)==FALSE)||(n<=0))
    stop("invalid arguments")
  
  #creates a numeric vector of length n called f
  f<-numeric(n)
  
  #assigns n random chi-squared deviates to u and to v with degrees of freedom
  #df1 and df2 respectively by calling the function my.rchisq
  u<-my.rchisq(n,df=df1) 
  v<-my.rchisq(n,df=df2) 
  
  #for i=1,...,n calculates the ith random f-distributed deviate & stores it in
  #element i of vector f       
  for (i in 1:n){
    f[i]<-(u[i]/df1)/(v[i]/df2)
  }
  
  #returns the vector of pseudo-random F-distributed deviates
  return(f)
}
  


#below are my test functions

test.fitnorm<-function(n,mean=0,sd=1){
#Purpose: Plots a normal probabilty plot for comparing random normal deviates from 
#my.rnorm function with the normal distribution. The qq-plot should be evenly distributed 
#around the straight line if the data is from a normal distribution.
#Inputs: 
# n - number of values to return
# mean - mean of the values to return - default 0
# sd - standard deviation of the values to return - default 1
  a<-my.rnorm(n,mean,sd)
  qqnorm(a)
  qqline(a)
}

test.myrchisq<-function(n,df=1){
#Purpose: Tests all random chi-squared deviates are positive.
#Inputs: 
# n - number of values to return - no default
# df - degrees of freedom - default 1
#Outputs: 
# pass.test - takes on the value TRUE if all chi-squared deviates are positive
  x<-my.rchisq(n,df)
  pass.test<-(x>0)
  return(pass.test)
}

test.myrf<-function(n,df1=1,df2=1){
#Purpose: Tests all the random f-distributed deviates are positive.
#Inputs: 
# n - number of values to return - no default
# df1 - degrees of freedom of the numerator - default 1
# df2 - degrees of freedom of the denominator - default 1
#Outputs: 
# pass.test - takes on the value TRUE if all chi-squared deviates are positive.
  x<-my.rf(n,df1,df2)
  pass.test<-(x>0)
  return(pass.test)
}

test.input<-function(){
#Purpose: Checks the mean & sd of output from my.rnorm.  Calculates the mean and sd of 
#the random normal deviates generated in my.rnorm. We look to see that the mean and sd
#returned by this function are equal to those entered as arguments in the my.rnorm command
#Inputs: None
#Outputs: 
# mean1 - the mean of the random normal deviates
# sd1 - the standard deviation of the random normal deviates
  x<-my.rnorm(n=10000,mean=3,sd=4)
  mean1<-mean(x)
  sd1<-sd(x)
  ret<-list(mean=mean1,sd=sd1)
  return(ret)
}

test.norm<-function(n,mean=0,sd=1){
#Purpose: Tests for valid output from my.rnorm. Checks that the number of values 
#outputted by the function are equal to the number requested by the user and checks that
#the output contains numeric values.
#Inputs: 
# n - number of values to return
# mean - mean of the values to return - default 0
# sd - standard deviation of the values to return - default 1
#Outputs:
# pass.test - takes on the value TRUE if 'x' contains n numeric values
  x<-my.rnorm(n,mean,sd)
  pass.test<-(length(x)==n & is.numeric(x))
  return(pass.test)
}

test.chisq<-function(n,df=1){
#Purpose: Tests for valid output from my.rchisq. Checks that the number of values 
#outputted by the function are equal to the number requested by the user and checks that
#the output contains numeric values.
#Inputs: 
# n - number of values to return
# df - degrees of freedom - default 1
#Outputs:
# pass.test - takes on the value TRUE if 'x' contains n numeric values
  x<-my.rchisq(n,df)
  pass.test<-(length(x)==n & is.numeric(x))
  return(pass.test)
}

test.f<-function(n,df1=1,df2=1){
#Purpose: Tests for valid output from my.rf. Checks that the number of values 
#outputted by the function are equal to the number requested by the user and checks that
#the output contains numeric values.
#Inputs: 
# n - number of values to return
# df1 - degrees of freedom of the numerator - default 1
# df2 - degrees of freedom of the denominator - default 1
#Outputs:
# pass.test - takes on the value TRUE if 'x' contains n numeric values
  x<-my.rf(n,df1,df2)
  pass.test<-(length(x)==n & is.numeric(x))
  return(pass.test)
}
