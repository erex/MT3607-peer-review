#I confirm that the attached is my own work, except where clearly indicated in 
# the text

my.rnorm<-function(n,mean=0,sd=1) {
#Purpose: Uses the uniform random number generator in R to generate 
# pseudo-random values from a normal distribution
#Inputs:
# n - number of values to return - a numeric scalar (n is an integer)
# mean - mean of values to return - default 0
# sd - standard deviation of values to return - default 1
#Outputs:
# A vector of pseudo-random values from a normal distribution
  
  if (n<=0) stop("invalid arguments, n must be a positive number")
  if (sd<0) stop("invalid arguments, standard deviation must be a positive number")

  # Creates a vector of length n
  x<-numeric(n)
    
  # Loop performs algorithm to generate normally-distributed deviates, using the 
  # Central limit theorem method, and stores them in the vector x
  for (i in 1:n) {
      
    # Loop uses the R uniform random number generator for the algorithm 
    u<-runif(16,mean,sd)
    s<-sum(u)
    x[i]<-(s-8)*(sqrt(12/16))
  }
    
  # Prints a vector of pseudo-random normal deviates
  return(x)
}


my.rchisq<-function(n,df=1) {
#Purpose: Generates pseudo-random chi-squared distributed deviates from random
# normal deviates
#Inputs:
# n - number of values to return - a numeric scalar (n is an integer)
# df - degrees of freedom of the distribution - default 1
#Outputs:
# A vector of pseudo-random values from a chi-squared distribution
  
  if (n<=0) stop("invalid arguments, n must be a positive number")
  if (df<=0) stop("invalid arguments, df must be a positive number")
  
  # Creates a vector of length n
  x<-numeric(n)
  
  # Loop performs algorithm to generate chi-squared distributed deviates and 
  # stores them in the vector x
  for (i in 1:n) {
    
    # Loop uses my.rnorm function to generate random normal deviates for the
    # algorithm 
    z<-(my.rnorm(df))^2
    x[i]<-sum(z)
  }
  
  # Prints a vector of pseudo-random chi-squared distributed deviates
  return(x)
}


my.rf<-function(n,df1=1,df2=1) {
#Purpose: Generates pseudo-random F-distributed deviates from chi-squared 
# distributed deviates
#Inputs:
# n - number of values to return - a numeric scalar (n is an integer)
# df1 - degrees of freedom of the numerator - default 1
# df2 - degrees of freedom of the denominator - default 1
#Outputs:
# A vector of pseudo-random F-distributed deviates
  
  if (n<=0) stop("invalid arguments, n must be a positive number")
  if (df1<=0) stop("invalid arguments, df1 must be a positive number")
  if (df2<=0) stop("invalid arguments, df2 must be a positive number")
  
  # Uses my.rchisq function to generate two vectors of chi-squared distributed 
  # deviates 
  u<-my.rchisq(n,df1)
  v<-my.rchisq(n,df2)
  
  # The two vectors are then used in the distribution theory theorem for 
  # F-distributed deviates
  f<-(u/df1)/(v/df2)
  
  # Prints a vector of pseudo-random F-distributed deviates
  return(f)
}


large.n<-function(a) {
#Purpose: Test whether functions still output a vector of n numeric values when n
# is a random large integer
#Inputs:
# a - user-defined function, either "my.rnorm", "my.chisq" or "my.rf"
#Outputs:
# Returns "Pass" if the function has created a vector of n numeric values and 
# "Fail" if it has not, also prints value of n
  
  # Samples a random large number for n, in the range of (1000,10000) with
  # replacement
  n<-sample(1000:10000,1,replace=T)
  
  # Uses function given in input for a
  f<-a(n)
  
  # The if statement returns "PASS" if the function has created a vector of n 
  # numeric values and "FAIL" if it hasn't, also returns value of n 
  if(length(f)==n & is.numeric(f)) {
    cat("PASS","n=",n,"\n")
  } else {
    cat("FAIL","n=",n,"\n")
  }
}
