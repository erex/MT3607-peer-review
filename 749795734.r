# I confirm that the attached is my own work, except where clearly indicated in the text.


# purpose: to write a function that returns a vector of pseudo-random values from a normal distribution
# inputs: the number of values to return (n), mean (mean - default 0) and standard deviation (sd - default 1)
# outputs: a vector with n pseudo-random variables from a normal distribution

# defining function and inputs:
my.rnorm <- function(n,mean=0,sd=1){
  
  # setting mean as a single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  if ((length(mean)==1)==T & is.numeric(mean)==T){
  
  # setting sd as a non-negative single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  if (sd >= 0 & (length(sd)==1)==T & is.numeric(sd)==T){
  
  # setting n as a positive integer and a single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  g <- n-round(n) # defining a function that subtracts the rounded value for n (round(n)) from n itself. If n is an integer, the function will equal 0
  if (n > 0 & is.numeric(n)==T & (g==0)==T & (length(n)==1)==T)
{  
    
    # calculating a random variable x by using the given algorithm, repeating this n times (with the for-loop) and writing the values into a vector via x<-c(1:n) and [i]
    # in this function one of my colleagues helped me with defining the vector (x<-c(1:n) and [i])
    x<-c(1:n)
    for(i in 1:n){
      x [i] <- c(((sum(runif(16))-8)*(sqrt(12/16)))*sd + mean) 
    } 
    return (x)
  } else {
    cat("ERROR: Invalid argument")
  }
  } else {
    cat("ERROR: Invalid argument")
  } 
  } else {
    cat("ERROR: Invalid argument")
  } 
}


# purpose: to write a function that returns a vector of pseudo-random values from a chi squared distribution
# inputs: the number of values to return (n) and the degrees of freedom (df - default 1)
# outputs: a vector with n pseudo-random variables from a chi squared distribution

# defining function and inputs:
my.rchisq <- function(n,df=1){
  
  # setting df as a non-negative integer and a single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  h <- df-round(df)
  if (df > 0 & (h==0)==T & (length(df)==1)==T & is.numeric(df)==T){  
  
  # setting n as a positive integer and a single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  g <- n-round(n)
  if (n > 0 & (g==0)==T & is.numeric(n)==T & (length(n)==1)==T)
{  
    
    # calculating a random variable x by using the given algorithm and the former defined my.rnorm-function, repeating this n times (with the for-loop) and writing the values into a vector via x<-c(1:n) and [i]
    x<-c(1:n)
    for (i in 1:n)
      x [i] <- sum(exp(my.rnorm(n)))
    return (x)
} else {
  cat("ERROR: Invalid argument")
} 
} else {
  cat("ERROR: Invalid argument")
}
}


# purpose: to write a function that returns a vector of pseudo-random values from a F-distribution
# inputs: the number of values to return (n), the degrees of freedom of the nomerator (df1 - default 1) and the degrees of freedom of the denominator (df2 - default 1)
# outputs: a vector with n pseudo-random variables from a F-distribution

# defining function and inputs:
my.rf <- function(n, df1=1, df2=1){
  
  # setting de df's as non-negative integers and single values; if these are not correct, the message "ERROR: Invalid argument" will be printed
  i <- df1-round(df1)
  if (df1 > 0 & (i==0)==T & is.numeric(df1)==T & (length(df1)==1)==T){ 
  
  j <- df2-round(df2)
  if (df2 > 0 & (j==0)==T & is.numeric(df2)==T & (length(df2)==1)==T){ 
  
  # setting n as a positive integer and a single value; if this is not correct, the message "ERROR: Invalid argument" will be printed
  g <- n-round(n)
  if (n > 0 & (g==0)==T & is.numeric(n)==T & (length(n)==1)==T){
    
    # calculating a random variable x by using the given algorithm and the former defined my.rchisq-function, repeating this n times (with the for-loop) and writing the values into a vector via x<-c(1:n) and [i]
    x<-c(1:n)
    for (i in 1:n){
      x [i] <- (sum(my.rchisq(df1))/df1)*(df2/sum(my.rchisq(df2)))
    }  
    return (x)
  } else {
    cat ("ERROR: Invalid argument")
  } 
} else {
  cat ("ERROR: Invalid argument")
} 
} else {
  cat("ERROR: Invalid argument")
}
}


# Testing functions

# purpose: to check if the vectors created by the functions contain random values, meaning that every calculation should give different values.
# input: the my.rnorm/my.rchisq/my.rf function and a positive integer and single value for the number of values to be returned (=n)
# output: TRUE if the vectors are different, meaning they contain random values, or FALSE if they are equal, meaning they contain the same values

#x <- my.rnorm(n)
#y <- my.rnorm(n)
test.random <- function (x,y) {
  (x-y)
  (z=0)==F
}

#test.random(x,y)

# purpose: to test if any other value then a positive integer for n gives an error message
# input: the my.rnorm/my.rchisq/my.rf function and a non-positive non-integer for n
# output: FALSE if the function do not produce a vector, TRUE if they do

#is.numeric(my.rnorm(n))
