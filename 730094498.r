#I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm <- function(n,mean=0,sd=1){
#Purpose: Generate a vector of random normal deviates from a normal distribution
# Using central limit theorem method (Rubinstein 1981:89-90)
#Inputs:
# n = number of values to return
# mean = mean of values to return (default = 0) 
# sd = standard deviation of values to return (default = 1)
#Outputs: A vector of n random normal deviates
  
  #Detects incorrect inputs and stops the program execution, printing invalid arguments 
  if(missing(n)||length(n)>1||is.numeric(n)==FALSE||n<=0||(n-round(n))!=0)
    stop("invalid arguments")
  if(length(mean)>1||is.numeric(mean)==FALSE)
    stop("invalid arguments")
  if(length(sd)>1||is.numeric(sd)==FALSE||sd<0)
    stop("invalid arguments")
  
  #The following line assigns ans.1 to be a numeric object of size n
  ans.1 <- numeric(n)
  
  #A for loop run n times which carries out the Central limit theorem method (Rubinstein 1981:89-90), 
  # generating a single value from a normal distribution and assigning it to the ith position of ans.1. 
  for(i in 1:n){
    U <- runif(16)
    X <- (sum(U)-8)*sqrt(12/16)
    ans.1[i] <- (X*sd)+mean
  }
  
  #Returns a vector of n random normal deviates
  return(ans.1)
}


my.rchisq <- function(n,df=1){
#Purpose: Generates a vector of random chi-squared distributed deviates
# Using theorem for chi-squared distributed deviates (Larsen and Marx 1981:284)
#Inputs:
# n = number of values to return
# df = degrees of freedom of the distribution (default = 1)
#Outputs: A vector of n chi-sqaured deviates
  
  #Detects incorrect inputs and stops the program execution, printing invalid arguments 
  if(missing(n)||length(n)>1||is.numeric(n)==FALSE||n<=0||(n-round(n))!=0)
    stop("invalid arguments")
  if(length(df)>1||is.numeric(df)==FALSE||df<0||(df-round(df))!=0)
    stop("invalid arguments")
  
  #The following line assigns ans.2 to be a numeric object of size n
  ans.2 <- numeric(n)
  
  #A for loop run n times that calls the function my.rnorm, generating df random normal deviates
  # the values are squared and summed generating n chi-sqaured distributed deviates 
  for(i in 1:n){
    Z <- (my.rnorm(df))^2
    ans.2[i] <- sum(Z)
  }
  
  #Returns a vector of n chi-sqaured distributed deviates
  return(ans.2)
}


my.rf <- function(n, df1=1, df2=1){
#Purpose: Generates a vector of random F-distributed deviates
# Using distribution theory theorem for F-distributed deviates (Larsen and Marx 1981:293)
#Inputs:
# n = number of values to return
# df1 = degrees of freedom for numerator (default = 1)
# df2 - degrees of freedom for denominator (default = 1)
#Outputs: A vector of n random F-distributed deviates
  
  #Detects incorrect inputs and stops the program execution, printing invalid arguments 
  if(missing(n)||length(n)>1||is.numeric(n)==FALSE||n<=0||(n-round(n))!=0)
    stop("invalid arguments")
  if(length(df1)>1||is.numeric(df1)==FALSE||df1<0||(df1-round(df1))!=0)
    stop("invalid arguments")
  if(length(df2)>1||is.numeric(df2)==FALSE||df2<0||(df2-round(df2))!=0)
    stop("invalid arguments")
  
  #The following 3 lines assign A, B and ans.3 to be numeric objects of size n
  A <- numeric(n)
  B <- numeric(n)
  ans.3 <- numeric(n)
  
  #A for loop run n times generating chi-squared distributed deviates by calling my.rchisq
  # A chi-squared deviate from a distribution with df1 degrees of freedom is divided by df1
  # A chi-squared deviate from a distribution with df2 degrees of freedom is divided by df2
  # These two values are divided by one another and placed in the ith position in ans.3
  for(i in 1:n){
    A <- (my.rchisq(n,df=df1))
    B <- (my.rchisq(n,df=df2))
    ans.3[i] <- (A[i]/df1)/(B[i]/df2)
  }
  
  #Returns a vector of n random F-distributed deviates 
  return(ans.3)
}


my.test1 <- function(){
#Purpose: A function to test that my.rnorm, my.rchisq and my.rf are returning vectors of the
# desired length with numeric values.
#Inputs: No arguments required
#Outputs: 
# A vector with results for each function:
#  "TRUE" if test is passed,"FALSE" if test is failed
 
  pass.test<-character(3)
  
  x<-my.rnorm(10)
  pass.test[1]<-(length(x)==10 & is.numeric(x))
  
  y<-my.rchisq(10)
  pass.test[2]<-(length(y)==10 & is.numeric(y))
  
  z<-my.rnorm(10)
  pass.test[3]<-(length(z)==10 & is.numeric(z))
  
  return(pass.test)
}


my.test2 <- function(){
#Purpose: Tests whether the values being generated by my.rchisq and my.rf are positive
#Inputs: No arguments required
#Outputs:
# A vector with the results for the ith position of both functions
#  "TRUE" if both values are positive
#  "FALSE" if one or both of the values are negative
 
  x<-my.rchisq(10)
  y<-my.rf(10)
  
  pass.test<-character(10)
  
  for(i in 1:10){
    pass.test[i] <- (x[i] & y[i])>0
  }
  
  return(pass.test)
}


my.test3 <- function(){
#Purpose: To carry out a shapiro wilk test on the values generated by my.rnorm 
# (i.e.to test for normality)
#Inputs: No arguments required
#Outputs:
# The number of p-values out of 20 tests that were less than 0.05 
#  (i.e. would reject null hypothesis at 5% level)
 
  n<-0
  
  for(i in 1:20){
    norm.test <- shapiro.test(my.rnorm(100))
    if(norm.test[2]<0.05){
      n <- n + 1
    } else {
      n <- n + 0
    }
  }
  
  cat(n, "out of 20 p-values were less than 0.05","\n")
}


my.test4 <- function(){
#Purpose: To carry out a runs test on the values generated by my.rnorm 
# (i.e.to test for randomness)
#Inputs: No arguments required
#Outputs:
# The number of p-values out of 20 tests that were less than 0.05 
#  (i.e. would reject null hypothesis at 5% level)
  n<-0
  library(TSA)
  
  for(i in 1:20){
    x <- my.rnorm(1000)
    random.test <- runs(x,median(x))
    if(random.test[1]<0.05){
      n <- n + 1
    } else {
      n <- n + 0
    }
  }
  
  cat(n, "out of 20 p-values were less than 0.05","\n")
}
