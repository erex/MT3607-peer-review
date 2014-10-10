#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n, mean=0, sd=1){
  #Purpose: Creates n pseudo-random values from a normal distribution with mean, mean,
  # and standard deviation, sd, using an algorithm and returns as a vector of n elements.
  #Inputs:
  # n - a numeric scalar integer greater than or equal to 1
  # mean - a numeric scalar
  # sd - a numeric scalar greater than or equal to 0
  #Outputs:
  # a vector containing n pseudo-random values from a normal distribution
  
  #Error checking, stop and report error if:-
  # any input is not numeric, n is not integer or less than 1, standard deviation is less than 0 or any input is a vector
  #Class statement required in integer check to only check numerics for this property, otherwise error reported
  if (class(c(n, mean, sd))!="numeric" | (class(n)=="numeric" & n%%1!=0) | n<1 | sd<0 | length(c(n, mean, sd))!=3){
    stop("invalid arguments")
  }
  
  randnum<-rnorm<-vector()
  
  #Run nested loop n times which will calculate 16 random values and then using
  # an algoritm turn them into a pseudo-random normally distributed value
  for (i in 1:n) {
    for (j in 1:16){
      randnum[j]<-runif(1)
    }
    rnorm[i]<-(sum(randnum)-8)*sqrt(12/16)
  }
  
  #transform the standard normally distributed values to the normal distribution required
  rnorm<-rnorm*sd+mean
  
  return(rnorm)
}

my.rchisq<-function(n, df=1){
  #Purpose: Creates n pseudo-random values from a chi-squared distribution with degrees of 
  # freedom, df, using an algorithm and returns as a vector of n elements.
  #Inputs:
  # n - a numeric scalar integer greater than or equal to 1
  # df - a numeric scalar integer greater than or equal to 1
  #Outputs:
  # a vector containing n pseudo-random values from a chi-squared distribution
  
  #Error checking, stop and report error if:-
  # any input is not numeric, n or df is not integer, n or df is less than 1 or any input is a vector
  #Class statement required in integer check to only check numerics for this property, otherwise error reported
  if (class(c(n, df))!="numeric" | class(c(n, df))=="numeric" & (n%%1!=0 || df%%1!=0) | n<1 | df<1 | length(c(n, df))!=2){
    stop("invalid arguments")
  }
  
  #Run n times the loop to get df number of pseudo-random standard normally distributed values,
  # square them and then sum them to get value from chi-squared distribution
  rchi<-vector()
  for (i in 1:n){
    rchi[i]<-sum(my.rnorm(df)^2)
  }
  return(rchi)
}

my.rf<-function(n, df1=1, df2=1){
  #Purpose: Creates n pseudo-random values from an F-distribution with df1 and df2 degrees of 
  # freedom using an algorithm and returns as a vector of n elements.
  #Inputs:
  # n - a numeric scalar integer greater than or equal to 1
  # df1 - a numeric scalar integer greater than or equal to 1
  # df1 - a numeric scalar integer greater than or equal to 1
  #Outputs:
  # a vector containing n pseudo-random values from an F-distribution
  
  
  #Error checking, stop and report error if:-
  # any input is not numeric, n, df1 or df2 is not integer, n, df1 or df2 is less than 1 or any input is a vector
  #Class statement required in integer check to only check numerics for this property, otherwise error reported
  if (class(c(n, df1, df2))!="numeric" | class(c(n, df1, df2))=="numeric" & (n%%1!=0 | df1%%1!=0 | df2%%1!=0) | n<1 | df1<1 | df2<1 | length(c(n, df1, df2))!=3){
    stop("invalid arguments")
  }
  
  #Run n times the loop to get the ratio between two psuedo-random chi-squared values
  # divided by their degrees of freedom
  rf<-vector()
  for (i in 1:n){
    rf[i]<-((my.rchisq(1,df1)/df1)/(my.rchisq(1,df2)/df2))
  }
  return(rf)
}

zero.sd.test<-function(){
  #Purpose: Tests that when the my.rnorm function receives a standard deviation of zero
  # all n values produced are equal to the mean value received by the function
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  x<-my.rnorm(10,0,0)
  y<-my.rnorm(10,5,0)
  
  #Checks to see if the values produced are equal to the mean in both cases
  pass.test<-ifelse(all(x==rep(0,10)) & all(y==rep(5,10)), "PASS", "FAIL")
  return (pass.test)
}

num.length.test<-function(){
  #Purpose: Tests that when the functions: my.rnorm; my.rchisq; my.rf receive a value of 10 for n
  # the resulting returned value is equal to (n=10) and is of numeric class
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  x<-my.rnorm(10)
  y<-my.rchisq(10)
  z<-my.rf(10)
  
  #Checks to see if the vectors produced are equal to (n=10) in length and of class numeric
  pass.test<-ifelse(length(x)==10 & is.numeric(x) & length(y)==10 & is.numeric(y) & length(z)==10 & is.numeric(z), "PASS", "FAIL")
  return (pass.test)
}

num.null.test<-function(){
  #Purpose: Tests that when the functions: my.rnorm; my.rchisq; my.rf receive a numeric value for n which should
  # not be accepted an error message is returned (using the try function and silencing the error message)
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  a<-c(0, -5, 2.5)
  
  #Sets the counter for each function equal to zero
  x<-y<-z<-0
  
  #Runs a loop for each of the numeric values in 'a' to check if an error message would be produced 
  for (i in 1:3){
    if (class(try(my.rnorm(a[i]), silent=TRUE))=="try-error") x<-x+1
    if (class(try(my.rchisq(a[i]), silent=TRUE))=="try-error") y<-y+1
    if (class(try(my.rf(a[i]), silent=TRUE))=="try-error") z<-z+1
  }
  
  #Checks to see that every case has returned an error (which it must to pass the test)
  pass.test<-ifelse(sum(x)==3 & sum(y)==3 & sum(z)==3, "PASS", "FAIL")
  return (pass.test)
}

char.null.test<-function(){
  #Purpose: Tests that when the functions: my.rnorm; my.rchisq; my.rf receive a character value for n which should
  # not be accepted an error message is returned (using the try function and silencing the error message)
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  a<-c("word", "", "$")
  
  #Sets the counter for each function equal to zero
  x<-y<-z<-0
  
  #Runs a loop for each of the numeric values in 'a' to check if an error message would be produced
  for (i in 1:3){
    if (class(try(my.rnorm(a[i]), silent=TRUE))=="try-error") x<-x+1
    if (class(try(my.rchisq(a[i]), silent=TRUE))=="try-error") y<-y+1
    if (class(try(my.rf(a[i]), silent=TRUE))=="try-error") z<-z+1
  }
  
  #Checks to see that every case has returned an error (which it must to pass the test)
  pass.test<-ifelse(sum(x)==3 & sum(y)==3 & sum(z)==3, "PASS", "FAIL")
  return (pass.test)
}

vector.null.test<-function(){
  #Purpose: Tests that when the functions: my.rnorm; my.rchisq; my.rf receive a numeric vector for n which should
  # not be accepted an error message is returned (using the try function and silencing the error message)
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  a<-c(0, -5, 2.5)

  #Checks to see that none of the functions accept a numeric vector for argument n, returns PASS if passed test, FAIL otherwise
  pass.test<- ifelse(class(try(my.rnorm(a), silent=TRUE))=="try-error" & class(try(my.rchisq(a), silent=TRUE))=="try-error" & class(try(my.rf(a), silent=TRUE))=="try-error", "PASS", "FAIL")
  return (pass.test)
}

rnorm.test<-function(){
  #Purpose: To compare the mean values produced by my.rnorm to the rnorm function for large n as they
  # should be similar
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  # [the difference over large n was suitably small to pass the test]
  
  n<-1000
  max<-0
  
  #Runs a loop to compare mean values for n values from each distribution and takes the maximum of these means
  for (i in 1:10){
    x<-mean(my.rnorm(n, sd=1))-mean(rnorm(n, sd=1))
    max<-ifelse(x>max, x, max)
  }
  pass.max<-0.2
  
  #If the maximum value is less than pass.max then the test is passed 
  pass.test<-ifelse(abs(max)<pass.max, "PASS", "FAIL")
  return (pass.test)
}

rchisq.test<-function(){
  #Purpose: To compare the mean values produced by my.rchisq to the rchisq function for large n as they
  # should be similar
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  # [the difference over large n was suitably small to pass the test]
  
  n<-1000
  max<-0
  
  #Runs a loop to compare mean values for n values from each distribution and takes the maximum of these means
  for (i in 1:10){
    x<-mean(my.rchisq(n, df=1))-mean(rchisq(n, df=1))
    max<-ifelse(x>max, x, max)
  }
  pass.max<-0.2
  
  #If the maximum value is less than pass.max then the test is passed 
  pass.test<-ifelse(abs(max)<pass.max, "PASS", "FAIL")
  return (pass.test)
}

valid.sd.test<-function(){
  #Purpose: Tests input values to the sd argument in my.rnorm to see if they are accepted
  # and rejected (when they should be)
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  a<-c(-5, -2.5, -10)
  b<-c(0, 100, 2.5)
  
  #Sets the counters equal to zero
  x<-y<-0
  
  #Runs a loop for each of the numeric values in 'a' to check if an error message would be produced
  for (i in 1:3){
    if (class(try(my.rnorm(1, sd=a[i]), silent=TRUE))=="try-error") x<-x+1
    if (class(try(my.rnorm(1, sd=b[i]), silent=TRUE))=="try-error") y<-y+1
  }
  
  #Checks to see that every case has returned the correct class (which it must to pass the test)
  pass.test<-ifelse(sum(x)==3 & sum(y)==0, "PASS", "FAIL")
  return (pass.test)
}

valid.df.test<-function(){
  #Purpose: Tests input values to the df/df1/df2 arguments in my.rchisq and my.rf to see if they are rejected as they should be
  #Inputs: none
  #Outputs:
  # a value (pass.test) which states if the test was passed (PASS) or not (FAIL)
  
  a<-c(0, -5, 2.5)
  
  #Sets each of the counters equal to zero
  x<-y<-z<-0
  
  #Runs a loop for each of the numeric values in 'a' to check if an error message would be produced 
  for (i in 1:3){
    if (class(try(my.rchisq(1, df=a[i]), silent=TRUE))=="try-error") x<-x+1
    if (class(try(my.rf(1, df1=a[i]), silent=TRUE))=="try-error") y<-y+1
    if (class(try(my.rf(1, df2=a[i]), silent=TRUE))=="try-error") z<-z+1
  }
  
  #Checks to see that every case has returned an error (which it must to pass the test)
  pass.test<-ifelse(sum(x)==3 & sum(y)==3 & sum(z)==3, "PASS", "FAIL")
  return (pass.test)
}