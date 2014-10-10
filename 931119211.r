#YIQI CHEN 110001525
#I confirm that the attached is my work, except where clearly indicated in the text.


my.rnorm<- function(n,mean=0,sd=1){
  if(!(length(n)==1L & is.numeric(n))) { stop("the number of values to return must be a positive integer")}
  if(!(length(mean)==1L & is.numeric(mean))) { stop("the mean must be a real scalar")}
  if(!(length(sd)==1L & is.numeric(sd))) { stop("the standard deviation must be a positive scalar")}
  if (n!=ceiling(n)|(n<= 0)){stop("the number of values to return must be a positive integer")}
  if (sd<= 0){stop("the standard deviation must be strictly positive")}  
  # n must be a positive integer, sd must be positive real number, mean must be a real number
  X = rep(0,times=10);
  for (i in 1:n){
    U = runif(16);
    X[i] = ((sum(U)-8)*sqrt(3)/2)*sd+mean
  # the unifom random variables are generated in a vector form and we take the sum subtracting the mean to approximate a standard normal
  }
  return (X)
}

my.rchisq<- function(n,df=1){
  if(!(length(n)==1L & is.numeric(n))) { stop("the number of values to return must be a positive integer")}
  if(!(length(df)==1L & is.numeric(df))) { stop("the degree of freedom must be a postive integer")}
  if (n!=ceiling(n)|(n<= 0)){stop("the number of values to return must be a positive integer")}
  if (df!=ceiling(df)|(df<= 0)){stop("the degree of freedom must be a positive integer")}
  # we check the input parameters are integers
  Z = rep(0,n)
  for (i in 1:n){
    Z[i] = sum(my.rnorm(df)^2)    
  # here we generate a vector of df normal distributed random variables then take sum of their component-wise square
  }  
  return(Z)
}

my.rf<-function(n,df1=1,df2=2){
  if(!(length(n)==1L & is.numeric(n))) { stop("the number of values to return must be a positive integer")}
  if(!(length(df1)==1L & is.numeric(df1))) { stop("the degree of freedom of the numerator must be a postive integer")}
  if(!(length(df2)==1L & is.numeric(df2))) { stop("the degree of freedom of the denominator must be a postive integer")}    
  if (n!=ceiling(n)|(n<= 0)){stop("the number of values to return must be a positive integer")}
  if (df1!=ceiling(df1)|(df1<= 0)){stop("the degree of freedom of the numerator must be a positive integer")}
  if (df2!=ceiling(df2)|(df2<= 0)){stop("the degree of freedom of the denominator must be a positive integer")}
  #again we check all the input parameters are integers
  F = rep (0,n)
  for (i in 1:n){
    U = sum(my.rchisq(1,df1))
    V = sum(my.rchisq(1,df2))
    F[i] = U/V/df1*df2
    #note U and V in the question are independent chi squared variables, F is simply the ratio of U and V, normalised by their repsective degrees of  freedom.
  }
  return(F)
}

# One of the standard tests for a sample against a known continuous distribution is Kolmogorov-Smirnov Test
# However, in a 2012 paper, Razali and Wah (see Wikipedia page on Shapiro-Wilk test) concluded
# Shapiro-Wilk test is more powerful than KS (and several other tests) for normal distribution, so we use this
result1 <- function(n){
  Y = rep(0,n)
  for (i in 1:n){
    X = my.rnorm (5000)
    Y[i]=shapiro.test(X)$p.value
  }
  return(Y)
}
hist(result1(1000), breaks=20)
# We can repeated test from our generator using Shapiro-Wilk test, for a truly normal sample
# we should expect to reject the null hypothesis the sample is normal roughly 10% of the time, but
# we find here from 1 simulation of 1000 runs of sample size of 5000, the null hypothesis is rejected
# more than 85 time at 5% significance level and more than 160 times at 10% significance level.
# The plot is generally skewed to the left, which suggests, this may not be a good model. Repeated
# simulation shows a similar result.

qqnorm(my.rnorm(10000))

# In addition, we can also use qq plots for a given sample, using qqnorm function in R. A typical
# normal distribution should have an approximately straightline, but the QQplot from our sample seems
# a little light tailed if we use a large sample size (for example 10000). This suggests the
# samples we generate are light tailed.This is not surprising as the random variables we generate
# are bounded by truly nomally distributed random variables are not.

# Likewise, we can do this type of repeated testing for K-S tests for Chi-Squared and F-distributions
result2 <- function(n,df){
  Y = rep(0,n)
  for (i in 1:n){
    X = my.rchisq (500,df)
    Y[i]=ks.test(X,pchisq,df)$p.value
  }
  return(Y)
}

hist(result2(1000,1), breaks=20)
hist(result2(1000,5), breaks=20)
hist(result2(1000,10), breaks=20)

#The histograms of p-values for different values of degree of freedom from KS tests is much more uniform
#than the result from Shapiro Wilk tests for normal distribution. It would appear from these plots that
#our way of sampling the Chi-squared distribution seems reasonable.

#Similarly, for F distributions
result3 <- function(n,df1,df2){
  Y = rep(0,n)
  for (i in 1:n){
    X = my.rf (500,df1,df2)
    Y[i]=ks.test(X,pf,df1,df2)$p.value
  }
  return(Y)
}

hist(result3(1000,1,1), breaks=20)
hist(result3(1000,5,3), breaks=20)
hist(result3(1000,2,4), breaks=20)

#The histograms seems to suggests our sampling method is also okay for F-distribution. The reason is
#probably that since Chi-squared distribution are sum of squares of independent normals and F-distributions
#are ratios of Chi-squared distributions up to a constant, the error 'cancels out', The guess would
#be that the higher degrees of freedom, the better our sampling scheme is.