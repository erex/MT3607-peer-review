#I confirm that the attached is my own work, except where clearly indicated in the text.



my.rnorm <- function(n,mean=0,sd=1) {   #defines the function
  if (n < 1) {                          #conditional statement testing values less than 1
    stop ("invalid arguments")          #stops program
  } else if (abs(n - round(n)) > 0) {   #conditional statement testing non-integer values
    stop ("invalid arguments")
  } else if (sd < 0) {
    stop ("invalid arguments")          #conditional statement testing negative standard deviation
  } else {
    x <- numeric()                      #sets x to be numeric
    for (i in 1:n) {                    #loops n times
      U <- numeric()
      for (j in 1:16) {
        U <- c(U,runif(1))              #adds pseudo-random value to U
      }
      x <- c(x,(( (sum(U)-8)*(0.75^0.5) )*sd) + mean)   #generates normally distributed deviates from U
    }
    return(x)                           #outputs normally distributed deviates
  }
}                                       #end function

my.rchisq <- function(n,df=1) {
  if (n < 1) {
    stop ("invalid arguments")
  } else if (abs(n - round(n)) > 0) {
    stop ("invalid arguments")
  } else {
    chisq <- numeric()
    for (i in 1:n) {
      chisq <- c(chisq,sum(rnorm(df)^2))    #generates chi squared distributed deviates
    }
    return(chisq)
  }
}

my.rf <- function(n,df1=1,df2=1) {
  if (n < 1) {
    stop ("invalid arguments")
  } else if (abs(n - round(n)) > 0) {
    stop ("invalid arguments")
  } else {
    Fdist <- U <- V <- numeric()        #sets Fdist, U and V to be numeric
    for (i in 1:n) {
      U <- sum(rnorm(df1)^2)            #generates df1 random normal deviates, squares them and then sums them
      V <- sum(rnorm(df2)^2)
      Fdist <- c(Fdist, (U/df1)/(V/df2) )   #generates F-distributed deviates
    }
    return(Fdist)
  }
}



my.rnormntest <- function(n) {          
  x <- my.rnorm(n)
  pass.test <- (length(x)==n & is.numeric(x))   #sets pass.test to be true if x contains n numeric values
  return(pass.test)
}
my.rnormntest(7)                        #runs the test function for value n=7

my.rchisqntest <- function(n) {         #similar test for my.rchisq function
  x <- my.rchisq(n)
  pass.test <- (length(x)==n & is.numeric(x))
  return(pass.test)
}
my.rchisqntest(7)

my.rfntest <- function(n) {             #similar test for my.rf function
  x <- my.rf(n)
  pass.test <- (length(x)==n & is.numeric(x))
  return(pass.test)
}
my.rfntest(7)

my.rnormmean12test <- function(m) {     #test correct mean when set to my arbitrary value of 12
  x<-0
  for (i in 1:m) {                      #loops m times (m is intended to be large)
    x <- c(x,my.rnorm(n=1,mean=12))
  }
  actual <- mean(x)
  return(actual)
}
my.rnormmean12test(10000)               #output value should be roughly equal to 12

my.rnormsd8.5test <- function(m) {      #similar test for the standard deviation
  x<-0
  for (i in 1:m) {
    x <- c(x,my.rnorm(n=1,sd=8.5))
  }
  actual <- sd(x)
  return(actual)
}
my.rnormsd8.5test(10000)                #output value should be roughly equal to 8.5