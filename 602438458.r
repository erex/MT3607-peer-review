#I conform that the attached is my own work, except where clearly indicated in the text.
#Sources for algorithms:
# Larsen, R.J. and M.L. Marx. 1981. An introduction to mathematical 
#   statistics and its applications. Prentice-Hall, Inc.
# Rubinstein, R.Y. 1981. Simulation and the Monte Carlo method. 
#   John Wiley and Sons.

my.rnorm <- function(n, mean=0, sd=1) {
  #Purpose: generates pseudo-random values from a normal distribution
  #Inputs:
  #   n - the number of values to generate
  #   mean - the mean of the normal distribution (default 0)
  #   sd - the standard deviation of the normal distribution (default 1)
  #Outputs:
  #   A vector of n values
  
  #check for invalid arguments
  if(!is.numeric(n) | !is.numeric(mean) | !is.numeric(sd)) { 
    #n, mean or sd is not numeric
    stop("invalid arguments")
  } else if(length(n) != 1 | length(mean) != 1 | length(sd) != 1) {
    #n, mean or sd is not a single value
    stop("invalid arguments")
  } else if(!is.finite(n) | !is.finite(mean) | !is.finite(sd)) {
    #n or mean or sd is not finite (or is NaN)
    stop("invalid arguments")
  } else if(as.integer(n) != n | n <= 0) { 
    #n is not a positive integer
    stop("invalid arguments")
  } else if(sd<0) {
    #standard deviation is negative
    stop("invalid arguments")
  }
  
  result <- c()
  for(i in 1:n) { #generate n values
    #generate 16 uniformly distributed random deviates
    deviates <- runif(16)
    
    #apply Rubinstein's method to generate a standard normally distributed deviate
    stnorm <- ( sum(deviates) - 8 ) * (sqrt(12/16))
    
    #adjust for given values of mean and standard deviation
    norm <- stnorm*sd + mean
    
    #insert value into result vector
    result <- c(result, norm)
  }
  return(result)
}

my.rchisq <- function(n, df=1) {
  #Purpose - generates pseudo-random values from a chi squared distribution
  #Inputs:
  #   n - number of values to generate
  #   df - degrees of freedom of the chi squared distribution (default 1)
  #Output:
  #   a vector of n values
  
  #check for invalid arguments
  if(!is.numeric(n) | !is.numeric(df)) {
    #n or df is not numeric
    stop("invalid arguments")
  } else if(length(n) != 1 | length(df) != 1) { 
    #n or df is not a single value
    stop("invalid arguments")
  } else if(!is.finite(n) | !is.finite(df)) {
    #n or df is not finite (or is NaN)
    stop("invalid arguments")
  } else if(as.integer(n) != n | n <= 0) {
    #n is not a positive integer
    stop("invalid arguments")
  } else if(as.integer(df) != df | df <= 0) {
    #df is not a positive integer
    stop("invalid arguments")
  } 
  
  result <- c()
  for(i in 1:n) { #generate n values
    #generate df standard normal deviates
    deviates <- my.rnorm(df)
    
    #use the result from Larsen and Marx to generate chi squared deviates
    chis <- sum(deviates^2)
    
    #insert value into result vector
    result <- c(result, chis)
  }
  return(result)
}

my.rf <- function(n, df1=1, df2=1) {
  #Purpose - generates pseudo-random values from an F distribution
  #Inputs:
  #   n - number of values to generate
  #   df1 - degrees of freedom of the numerator
  #   df2 - degrees of freedom of the denominator
  #Output:
  #   A vector of n values
    
  #check for invalid arguments
  if(!is.numeric(n) | !is.numeric(df1) | !is.numeric(df2)) {
    #n, df1 or df2 is not numeric
    stop("invalid arguments")
  } else if(length(n) != 1 | length(df1) != 1 | length(df2) != 1) { 
    #n, df1 or df2 is not a single value
    stop("invalid arguments")
  }else if(!is.finite(n) | !is.finite(df1) | !is.finite(df2)) {
    #n, df1 or df2 is not finite
    stop("invalid arguments")
  } else if(as.integer(n) != n | n <= 0) {
    #n is not a positive integer
    stop("invalid arguments")
  } else if(as.integer(df1) != df1 | df1 <= 0) {
    #df1 is not a positive integer
    stop("invalid arguments")
  } else if(as.integer(df2) != df2 | df2 <= 0) {
    #df2 is not a positive integer
    stop("invalid arguments")
  }
  
  result <- c()
  for(i in 1:n) { #generate n values
    #generate two chi-square deviates, with degrees of freedom df1 and df2
    u <- my.rchisq(1,df=df1)
    v <- my.rchisq(1,df=df2)
    
    #use these to generate an F-distributed deviate using result from Larsen and Marx
    newf <- (u/df1)/(v/df2)
    
    #insert this value into result vector
    result <- c(result,newf)
  }
  return(result)
}


my.testerrors <- function() {
  #prints a series of attempted function calls that should all return errors
  #string as argument
  print( tryCatch( my.rnorm("two"), error=return) ) 
  print( tryCatch( my.rf("3"), error=return) ) 
  #list as argument
  print( tryCatch( my.rnorm( c(3, 4, 5) ), error=return) )
  print( tryCatch( my.rchisq(100, c(4,5,6)), error=return) )
  print( tryCatch( my.rf(100, c(4,5,6), 7), error=return) )
  #negative numbers
  print( tryCatch( my.rnorm(-23), error=return) )
  print( tryCatch( my.rnorm(100, 4, -2), error=return) )
  print( tryCatch( my.rchisq(100, -20), error=return) )
  print( tryCatch( my.rf(100, 15, -2), error=return) )
  print( tryCatch( my.rf(100, -15, 2), error=return) )
  #non-integer values where integers required
  print( tryCatch( my.rnorm(3.5), error=return) )
  print( tryCatch( my.rchisq(0, 9), error=return) )
  print( tryCatch( my.rchisq(100, 0.4), error=return) )
  print( tryCatch( my.rf(100, 9.5, 2), error=return) )
  print( tryCatch( my.rf(100, 2, 9.5), error=return) )
  print( tryCatch( my.rf(100, 0.4, -2), error=return) )
}

my.testnorm <- function(n, mean, sd) {
  #tests how well my.rnorm conforms to the expected distribution
  #Inputs: same as my.rnorm
  mynorm <- my.rnorm(n,mean,sd)
  
  #generate histogram and Q-Q plot
  hist(mynorm)
  qqnorm(mynorm)
  qqline(mynorm)
  
  #summaries of the data
  cat("Values: ", length(mynorm), "\n")
  cat("Mean: ", mean(mynorm), "\n")
  cat("Standard deviation:", sd(mynorm), "\n")
}

my.testchisq <- function(n, df) {
  #tests how well my.rchisq conforms to the expected distribution
  #Inputs: same as my.rchisq
  mychi <- my.rchisq(n, df)
  
  #generate histogram
  hist(mychi)
  
  #summaries of the data
  cat("Values: ", length(mychi), "\n")
  cat("Mean: ", mean(mychi), "\n")
  cat("Variance: ", var(mychi), "\n")
}

my.testf <- function(n, df1, df2) {
  #tests how well my.rf conforms to the expected distribution
  #Inputs: same as my.rf
  myf <- my.rf(n, df1, df2)
  
  #generate histogram
  hist(myf)
  
  #summaries of the data
  cat("Values: ", length(myf), "\n")
  cat("Mean: ", mean(myf), "\n")
  cat("Variance: ", var(myf), "\n")  
}
