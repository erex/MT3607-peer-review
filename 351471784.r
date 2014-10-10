# MT3607, Assignment 1
# I confirm that the attached is my own work, except where clearly indicated in the text.
#
# This file includes 3 main functions
#   - my.rnorm(n, mean, sd)
#   - my.rchisq(n, df)
#   - my.rf(n, df1, df2)
#
# It also contains some functions for testing the main functions:
#   - test.my.rnorm()
#   - test.my.rchisq()
#   - test.my.rf()
#
# There is another function, for convenience which runs the three main 
# testing functions:
#   - test.asmt1()
#
# Additionally, there are two helper functions used to perform some of the tests:
#   - test.sample.sd.mean
#   - test.sample.mean



my.rnorm <- function(n, mean=0, sd=1) {
  # Generates random deviates from the normal distribution.
  #
  # Args:
  #   n: The number of deviates to return.
  #   mean: The mean of the distribution.
  #   sd: The standard deviation of the distribution.
  #
  # Returns:
  #   A vector of length n
  
  # like rnorm(), if n is 0, return numeric(0)
  if (n==0)
    return(numeric(0))
  # ensure that n is a number
  if (!is.numeric(n))  
    stop("invalid arguments")
  # ensure that mean and sd are numeric scalars.
  if (!is.numeric(mean) || !is.numeric(sd) || 
      length(mean) > 1 || length(sd) > 1) {
    stop("invalid arguments")
  }
  # ensure that sd is positive
  if (sd < 0) {
    stop("invalid arguemnts: Std. Deviation must be positive.")
  }
    
  x <- 0
  for(i in 1:n) {
    x[i] = ( (sum(runif(16))-8) * sqrt(12/16) * sd ) + mean
  }
  return(x)
}

my.rchisq <- function(n, df=1) {
  # Generates random deviates from the chi-squared distribution.
  #
  # Args:
  #   n: The number of deviates to return.
  #   df: The degrees of freedom of the distribution.
  #
  # Returns:
  #   A vector of length n
  
  # like rchisq(), if n is 0, return numeric(0)
  if (n==0)
    return(numeric(0))
  # ensure that n is a number
  if (!is.numeric(n))  
    stop("invalid arguments")
  # ensure that df is a numeric scalars.
  if (!is.numeric(df) || length(df) > 1) {
    stop("invalid arguments")
  }
  # ensure that df is positive
  if (df < 0) {
    stop("invalid arguemnts: df must be positive.")
  }
  
  x <- 0
  for(i in 1:n) {
    x[i] <- sum(my.rnorm(df) ^ 2)
  }
  return(x)
}

my.rf <- function(n, df1=1, df2=1) {
  # Generates random deviates from the F-distribution.
  #
  # Args:
  #   n: The number of deviates to return.
  #   df1: degrees of freedom of the numerator – default 1.
  #   df2: degrees of freedom of the denominator – default 1.
  #
  # Returns:
  #   A vector of length n
  
  # like rf(), if n is 0, return numeric(0)
  if (n==0)
    return(numeric(0))
  # ensure that n is a number
  if (!is.numeric(n))  
    stop("invalid arguments")
  # ensure that df1 and df2 are numeric scalars.
  if (!is.numeric(df1) || length(df1) > 1 || 
      !is.numeric(df2) || length(df2) > 1 ) {
    stop("invalid arguments")
  }
  # ensure that df1 and df2 are positive
  if (df1 <= 0 | df2 <= 0) {
    stop("invalid arguments: df1 and df2 must be greater than 0")
  }
  
  # We do all the calculations on the vector level which 
  # performs the operations on the composite elements.
  x <- (my.rchisq(n,df1) / df1) / (my.rchisq(n,df2) / df2)
  return(x)
}



############################################################
# Testing Functions
############################################################ 


test.sample.sd.mean <- function(sample, theoretical.sd, theoretical.mean, margin) {
  # Tests whether the standard deviation and mean are within a given margin
  # of theoretical values.
  # 
  # Args:
  #   sample: a sample given as a numerical vector.
  #   theoretical.sd: theoretical standard deviation
  #   theoretical.mean: theoretical mean
  #   margin: acceptable margin
  #
  # Returns:
  #   Logical value. True if sd and mean of the sample are within margin of theoretical
  #   values, false otherwise.
  
  sample.sd <- sd(sample)
  sample.mean <- mean(sample)
  
  diff.sd <- abs(sample.sd - theoretical.sd)
  diff.mean <- abs(sample.mean - theoretical.mean)
  
  cat("             sample: sd = ",sample.sd,", mean = ",sample.mean,"\n");
  cat("        theoretical: sd = ",theoretical.sd,", mean = ",theoretical.mean,"\n");
  cat("         difference: sd = ",diff.sd,", mean = ",diff.mean,"\n");
  if(diff.sd <= margin & diff.mean <= margin ) {
    cat("    PASSED: sample mean and sd within margin (",margin,") of theoretical values. \n");
    tests[1] = T
  } else {
    cat("    FAILED: sample mean and sd NOT within margin (",margin,") of theoretical values. \n");
    return(F)  
  }
}


test.sample.mean <- function(sample, theoretical.mean, margin) {
  # Tests whether the mean is within a given margin of the theoretical value.
  # 
  # Args:
  #   sample: a sample given as a numerical vector.
  #   theoretical.mean: theoretical mean
  #   margin: acceptable margin
  #
  # Returns:
  #   Logical value. True if mean of the sample is within margin of theoretical
  #   values, false otherwise.
  
  sample.mean <- mean(sample)
  diff.mean <- abs(sample.mean - theoretical.mean)
  
  cat("             sample: mean = ",sample.mean,"\n");
  cat("        theoretical: mean = ",theoretical.mean,"\n");
  cat("         difference: mean = ",diff.mean,"\n");
  if(diff.mean <= margin ) {
    cat("    PASSED: sample mean within margin (",margin,") of theoretical values. \n");
    tests[1] = T
  } else {
    cat("    FAILED: sample mean NOT within margin (",margin,") of theoretical values. \n");
    return(F)  
  }
}




test.my.rnorm <- function() {
  # Run a series of tests on my.rnorm() and print out the results to the user.
  #
  # Returns:
  #   True if all tests passed, False otherwise.
  
  tests <- 0
  
  cat("\n\n=========================================\n")
  cat("Testing my.rnorm() function.\n")
  cat("=========================================\n\n")
  
  test.margin <- 0.03
  
  cat("= TEST 1: Test sd and mean of my.rnorm(10000):\n");
  tests[1] <- test.sample.sd.mean(
    sample=my.rnorm(10000), 
    theoretical.sd=1,
    theoretical.mean=0,
    margin=test.margin)
  cat("\n")
  
  cat("= TEST 2: Test sd and mean of my.rnorm(10000,mean=5):\n");
  tests[2] <- test.sample.sd.mean(
    sample=my.rnorm(10000, mean = 5), 
    theoretical.sd=1,
    theoretical.mean=5,
    margin=test.margin)
  cat("\n")
  
  cat("= TEST 3: Test sd and mean of my.rnorm(10000,sd=10):\n");
  tests[3] <- test.sample.sd.mean(
    sample=my.rnorm(10000, sd = 10), 
    theoretical.sd=10,
    theoretical.mean=0,
    margin=test.margin*10)
  cat("\n")
  
  cat("= TEST 4: Test sd and mean of my.rnorm(10000,mean=43.2,sd=15):\n");
  tests[4] <- test.sample.sd.mean(
    sample=my.rnorm(10000, mean=43.2, sd=15), 
    theoretical.sd=15,
    theoretical.mean=43.2,
    margin=test.margin*15)
  cat("\n")
  
  cat("SUMMARY: my.rnorm() passed",sum(tests),"of",length(tests),"tests\n");
  if (sum(tests == 0)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}



test.my.rchisq <- function() {
  # Run a series of tests on my.rchisq() and print out the results to the user.
  #
  # Returns:
  #   True if all tests passed, False otherwise.
  
  tests <- 0
  
  cat("\n\n=========================================\n")
  cat("Testing my.rchisq() function.\n")
  cat("=========================================\n\n")
  
  # Note: In the chi-sq distribution, mean = df, and variance = 2 * df
  # this implies that sd = sqrt( 2 * df )
  
  test.margin <- 0.03
  
  cat("= TEST 1: Test sd and mean of my.rchisq(10000,df=3):\n");
  tests[1] <- test.sample.sd.mean(
    sample=my.rchisq(10000,df=3), 
    theoretical.sd=sqrt(2 * 3),
    theoretical.mean=3,
    margin=test.margin * sqrt(2 * 3))
  cat("\n")
  
  cat("= TEST 2: Test sd and mean of my.rchisq(10000,df=50):\n");
  tests[2] <- test.sample.sd.mean(
    sample=my.rchisq(10000,df=50), 
    theoretical.sd=sqrt(2 * 50),
    theoretical.mean=50,
    margin=test.margin * sqrt(2 * 50))
  cat("\n")
  
  cat("SUMMARY: my.rchisq() passed",sum(tests),"of",length(tests),"tests\n");
  if (sum(tests == 0)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

test.my.rf <- function() {
  # Run a series of tests on my.rf() and print out the results to the user.
  #
  # Returns:
  #   True if all tests passed, False otherwise.
  #
  # Note, I did some tests with both std deviation and mean, but 
  # in my test std deviation varied a lot and was not predictable enough
  # to test reliably.
  
  tests <- 0
  
  cat("\n\n=========================================\n")
  cat("Testing my.rf() function.\n")
  cat("=========================================\n\n")
  
  test.margin <- 0.02
  
  cat("= TEST 1: Test sd and mean of my.rf(10000,df1=9,df2=5):\n");
  tests[1] <- test.sample.mean(
    sample = my.rf(10000,df1=3,df2=5), 
    theoretical.mean = 5 / 3, # mean is df2 / (df2 - 2)
    margin=test.margin * 5)
  cat("\n")
  
  cat("= TEST 2: Test sd and mean of my.rf(10000,df1=7,df2=8):\n");
  tests[2] <- test.sample.mean(
    sample = my.rf(10000,df1=7,df2=8), 
    theoretical.mean = 8 / 6, # mean is df2 / (df2 - 2)
    margin=test.margin * 8)
  cat("\n")
  
  cat("SUMMARY: my.rf() passed",sum(tests),"of",length(tests),"tests\n");
  if (sum(tests == 0)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

test.asmt1 <- function() {
  test.my.rnorm()
  test.my.rchisq()
  test.my.rf()
}
