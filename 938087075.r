#I confirm that the attached is my own work, except where clearly indicated in the text.


# function my.rnorm (n, mean = 0, sd = 1)
#
# Returns a vector of n normally distributed pseudo-random variables with
# mean and standard deviation given by mean and sd
#
# @param n - integer denoting number of values to return
# @param mean - mean of the normal distribution
# @param sd - standard deviation of the normal distribution
#
# @return a vector containing n random variables ~ N(mean, sd)

my.rnorm <- function (n, mean = 0, sd = 1) {
    
    #test for valid function parameters:
    if (!is.numeric(n) || !is.numeric(mean) || !is.numeric(sd))
        stop ("invalid arguments")
    if (n != round(n))
        stop ("invalid arguments")
    if (sd < 0) 
        stop ("invalid arguments")
    if (n < 1) 
        stop ("invalid arguments")
  
    
    #vector of return values
    V <- vector(length = n)
  
    #calculate each pseudo-random normally distributed variable
    for (i in 1:n) {
      #method described in assignment sheet
        V[i] <- ( (sum(runif(16)) - 8) * sqrt(12/16) ) * sd + mean
    }  
  
    return (V)
}





# function my.rchisq (n, df = 1)
#
# Returns a vector of n chi-squared distributed pseudo-random variables with
# df degrees of freedom
#
# @param n - an integer denoting number of values to return
# @param df - an integer number, degrees of freedom for chisquared distribution
# 
# @return a vector containing n random variables ~ chisq(df)

my.rchisq <- function (n, df = 1) {
    
    #test for valid function parameters:
    if (!is.numeric(n) || !is.numeric(df))
        stop ("invalid arguments")
    if (n != round(n) || df != round(df))
        stop ("invalid arguments")
    if (df < 1) 
        stop ("invalid arguments")
    if (n < 1)
        stop ("invalid arguments")
  
    
    #vector of return values
    V = vector(length = n)
  
    for (i in 1:n) {
        #follow the definition of chisq distribution
        df.norm <- rnorm(df) 
        V[i] <- sum(df.norm * df.norm)
    }
  
    return (V)
}





# function my.rf (n, df1 = 1, df2 = 2)
#
# Returns a vector of n F distributed pseudo-random variables with
# df1 and df2 degrees of freedom
#
# @param n - an integer denoting number of values to return
# @param df1 - an integer number, degrees of freedom of numerator
# @param df2 - an integer number, degrees of freedom of denominator
#
# @return a vector containing n random variables ~ F(df1, df2)

my.rf <- function (n, df1 = 1, df2 = 1) {
    
    #test for valid function parameters:
    if (!is.numeric(n) || !is.numeric(df1) || !is.numeric(df1))
        stop ("invalid arguments")
    if (n != round(n) || df1 != round(df1) || df2 != round(df2))
        stop ("invalid arguments")
    if (df1 < 1 || df2 < 1) 
        stop ("invalid arguments")
    if (n < 1) 
        stop ("invalid arguments")
    
    
    #vector of return values
    V = vector(length = n)
    
    for (i in 1:n) {
        #follow the definition of F distribution
        V[i] <- ( my.rchisq(1, df1)[1] / df1 ) / 
                    ( my.rchisq(1, df2)[1] / df2 )
    }
    
    return (V)
}

# function test.random ()
#
# a function which performs a series of tests to validate the output 
# of my.rnorm, my.rchisq, my.rf
#
# @return none


test.random <- function () {

    # check whether the length of returned vector is as required
    if (length(my.rnorm(10)) != 10) stop ("incorrect length")
    if (length(my.rchisq(10, 2)) != 10) stop ("incorrect length")
    if (length(my.rf(10)) != 10) stop ("incorrect length")
    
    # check behaviour with invalid parameters, expecting a stop() raised error.
    
    # number of values is an integer, sd > 0, mean & sd are numeric.
    if (tryCatch(my.rnorm(10.5), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rnorm(65, 234, -2), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rnorm(65, FALSE, 2), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    
    
    #number of elements and degrees of freedoms are integers
    if (tryCatch(my.rchisq(10.5), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rchisq(10, 2.5), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rchisq(TRUE, 234), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")

    #number of elements and degrees of freedom are integers
    if (tryCatch(my.rf(10.5), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rf(10, 2.4, 4.3), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    if (tryCatch(my.rf(TRUE, 234, 506), error = function (e) {return ("error")}) != "error")
        stop ("bad parameter handling")
    
    # perform a runs test to see whether variables random (independent)
    # this actually tests the initial function runif() used. 
    install.packages("randtests")
    library("randtests")
    
    data <- my.rnorm(100)
    a <- runs.test(data, "two.sided", median(data))
    if (a["p.value"] < 0.01) stop ("independency failure")
    
    data <- my.rchisq(100, 3)
    a <- runs.test(data, "two.sided", median(data))
    if (a["p.value"] < 0.01) stop ("independency failure")
    
    data <- my.rf(100, 46, 94)
    a <- runs.test(data, "two.sided", median(data))
    if (a["p.value"] < 0.01) stop ("independency failure")
    
    
    #check whether the mean of normal variables is close to the required one
    data <- my.rnorm(1000, 12, 1)
    if (abs(mean(data) - 12) > 0.05) stop ("bad mean of my.rnorm")
    
    
    print ("All tests passed!")
}
