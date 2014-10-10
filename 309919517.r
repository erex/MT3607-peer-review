#I confirm that the attatched is my own work, except where clearly 
#indicated in the text
inputValidation <-function(input) {
  #Purpose: Used to check if the input to functions is valid.
  #Returns FALSE if a vector is a scalar integer between 1 and 
  #1000000, inclusive. Otherwise returns TRUE. 
  #Inputs:
  # input - the input which is being checked to be valid
  #Output: TRUE if input is invalid
  #        FALSE if input is valid
  #Example usage inside a function with parameter n:  
  #if (inputValidation(n)) stop("invalid arguments")
  if (length(input)!=1) return (TRUE)
  if (input%%1!=0 | input<1 | input>1000000) return (TRUE)
  return (FALSE)
}
my.rnorm <- function(n,mean=0,sd=1) { 
  #Purpose: Generate random values from a normal distribution. The function 
  #Randomly generates n normal deviates with mean equal to mean and standard
  #deviation equal to sd.
  #Inputs:
  # n - number of values to generate and return - no default
  # mean - the mean of the normal distribution from which to generate values
  # sd - the standard deviation of the normal distribution from which 
  # to generate values
  #Outputs:
  # rnorm - the n random values from the normal distribution specified by 
  # the inputs mean and sd
  if (inputValidation(n)) stop("invalid arguments") 
  #inputValidation returns TRUE if n is non-scalar, non-integer, less than 1 or
  #greater than 10^6, triggering an error message.
  rnorm<-numeric(n)
  for (i in 1:n) {
    rnorm[i]<-(sum(runif(16))-8)*sqrt(12/16)  #Central limit theorem method for generating a single standard normal deviate (Rubinstein 1981:89-90)
  } 
  rnorm<-rnorm*sd+mean #converts standard normal deviates to normal deviates with mean equal to mean and standard deviation equal to sd
  return(rnorm)
}
my.rchisq <- function(n,df=1) {
  #Purpose: Generate random values from a chi squared distribution. The function 
  #Randomly generates n deviates from the chi square distribution with specified
  #degree of freedom
  #Inputs:
  # n - number of values to generate and return - no default
  # df - the degrees of freedom of the chi square distribution from which to
  # generate values
  #Outputs:
  # rchisq - the n random values from the chi squared distribution specified by 
  # the input
  if (inputValidation(n)|inputValidation(df)) stop("invalid arguments")
  rchisq<-numeric(n)
  for (i in 1:n) {
    rnorm<-my.rnorm(df)
    rchisq[i]<-sum(rnorm^2)
  }
  return(rchisq)
}
my.rf <- function(n,df1=1,df2=1) {
  #Purpose: Generate random values from a F-distribution. The function 
  #Randomly generates n deviates from the F-distribution with specified
  #degrees of freedom.
  #Inputs:
  # n - number of values to generate and return - no default
  # Let X and Y be Chi-squared distributions with degrees of freedom df1 and df2
  # respectively. When this function takes the inputs df1 and df2 it generates
  # values from the F-distribution which is distributed as (X/df1)/(Y/df2).
  # df1 - the degrees of freedom of the chi-squared distribution in the numerator
  # of the F-distribution from which to generate values
  # df2 - the degrees of freedom of the chi-squared distribution in the denumerator
  # of the F-distribution from which to generate values
  #Outputs:
  # rchisq - the n random values from the chi squared distribution specified by 
  # the input
  if (inputValidation(n)|inputValidation(df1)|inputValidation(df2)) 
    stop("invalid arguments")
  rf<-numeric(n)
  rchisq1<-my.rchisq(n,df1)
  rchisq2<-my.rchisq(n,df2)
  for(i in 1:n) {
    rf[i]<-(df2*rchisq1[i])/(df1*rchisq2[i])
  }
  return(rf)
}
