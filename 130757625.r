
################################################################################################
#### I confirm that the attached is my own work, excpet where clearly indicated in the text.####
################################################################################################

# Algorithm for normally-distributed deviates

my.rnorm <- function ( n, mean = 0, sd = 1) {
  # Inputs: 
  # n - number of observations. If length(n) > 1, the length is taken to be the number required.
  # mean - vector of means.
  # sd- vector of standard deviations.
  # Outputs: 
  # a vector of normally-distributed deviates.
  
  # There will be an error message " Invalid arguments" when the fuction detects an incorrect input. 
  if ( n > 0 & n == round(n) ) {
    if ( sd >0 & sd == round(sd)) {
      
      # set the output device so it shows a summary object.
      dev.norm <- numeric(n)
      
      # do the calculation.
      i <- 1
      while (i <= n){
        u <- runif(16)
        dev.norm[i] <- sd * ((sum(runif(16))-8) * (sqrt(12/16))) + mean
        i <- i + 1
      }
      
      # Return a summary of the data.
      return (dev.norm)
    }
    else stop ("Invalid arguments: \"sd\" must be an integer >= 1")
  }
  else stop ("Invalid arguments: \"n\" must be an integer >= 1")
}



################################################################################################


# Theorem for Chisq-distributed deviates

my.rchisq <- function ( n, df=1) {
  # Inputs: 
  # n - number of observations. If length(n) > 1, the length is taken to be the number required.
  # df - degrees of freedom (non-negative and integer). 
  # Outputs: 
  # a vector of pseudo-random chi squared-distributed deviates.
  
  # There will be an error message " Invalid arguments" when the fuction detects an incorrect input.
  if ( n > 0 & n == round(n) ) {
    if ( df >0 & df == round(df)){
      
      # set the output device so it shows a summary object.
      dev.chisq <- numeric(n)
      
      # do the calculation.
      i <- 1
      while (i <= n){
        dev.chisq[i] <- sum(my.rnorm(df)^2)
        i <- i + 1
      }
      
      # Return a summary of the data.
      return (dev.chisq)
    }
    else stop ( "Invalid arguments: \"df\" must be an integer >= 1")
  }
  else stop ( "Invalid arguments: \"n\" must be an integer >= 1")
}


################################################################################################


# Distribution theory theorm for F-distributed deviates

my.rf <- function (n, df1=1, df2=1) {
  # Inputs: 
  # n - number of observations. If length(n) > 1, the length is taken to be the number required.
  # df1, df2 - degrees of freedom (non-negative and integer). 
  # Outputs: 
  # a vector of F-distributed deviates.
  
  # There will be an error message " Invalid arguments" when the fuction detects an incorrect input.
  if ( n > 0 & n == round(n) ) {
    if ( df1 >0 & df1 == round(df1)){
      if ( df2 >0 & df2 == round(df2)){
        
        # set the output device so it shows a summary object.
        dev.rf <- numeric(n)
        
        # do the calculation.
        i <- 1
        while (i <= n){
          dev.rf[i] <- (sum(my.rnorm(df1)^2)/df1) / (sum(my.rnorm(df2)^2)/df2)
          i <- i+1
        }
        
        # Return a summary of the data.
        return (dev.rf)
      }
      else stop ( "Invalid arguments: \"df\" must be an integer >= 1")
    }
    else stop ( "Invalid arguments: \"df\" must be an integer >= 1")
  }
  else stop ( "Invalid arguments: \"n\" must be an integer >= 1")
}


##################################### END OF THE FUNCTIONS #####################################


# Test of normal-distributed deviates.

test.norm <- function (x, mean=0, sd=1){
  
  # compare mean and sd.
  m.sample <- mean(x)
  sd.sample <- sd(x)
  cat ("Population mean =", mean, ", Sample mean =", m.sample, "\n",
       "Population sd =", sd, ", Sample sd =", sd.sample, "\n")
  
  # set the output device in one page
  par(mfrow = c(2,2))
  
  # histogram 
  hist(x, probability=T, main="Histogram")
  lines(density(x))
  
  # boxplot
  boxplot(x, main="Boxplot")
  
  # qq-plot
  qqnorm(x)
  
  # compare empirical cdfs
  plot(ecdf(x), do.points=F, verticals=T, main="Empirical cdfs")
  lines(ecdf(rnorm(length(x), mean, sd)), lty=3, do.points=F, verticals=T, col=2)
  
  # Selection of nomality tests
  if (length(x) > 3 & length(x) < 5000){
    shapiro.test(x) # Shapiro-Wilk Normality Test (for sample size between 3 and 5000).
  } 
  else 
    ks.test(x, "pnorm", mean, sd) # Kolmogorov-Smirnov Test
}


################################################################################################


# Test of chisq-distrbuted deviates

test.chisq <- function(x, df=1){
  
  # set the output device in one page
  par(mfrow=c(1,2))
  
  # Histogram
  hist(x, probability=T, main="Histogram")
  lines(density(x))
  
  # Compare empirical cdfs
  plot(ecdf(x), do.points=F, verticals=T, main="Empirical cdfs")
  lines(ecdf(rchisq(length(x), df)), lty=3, do.points=F, verticals=T, col=2)
  
  # Kolmogorov-Smirnov Test
  ks.test(x, "pchisq", df)
}


################################################################################################


# Test of F-distributed deviates

test.f <- function(x, df1=1, df2=1){
  
  # set the output device in one page
  par(mfrow=c(1,2))
  
  # Histogram
  hist(x, probability=T, main="Histogram")
  lines(density(x))
  
  # Compare empirical cdfs
  plot(ecdf(x), do.points=F, verticals=T, main="Empirical cdfs")
  lines(ecdf(rf(length(x), df1, df2)), lty=3, do.points=F, verticals=T, col=2)
  
  # Kolmogorov-Smirnov Test
  ks.test(x, "pf",df1, df2)
}

######################################################################
