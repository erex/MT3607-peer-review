#I confirm that the attached is my own work, except where clearly indicated in the text.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~my.rnorm, my.rchisq and my.rf functions~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: function for creating random deviates that follow a normal distribution
# using the Central Limit Theorem with deviates created from the R function for runif(0,1)
# with the parameters of the uniform deviates being 0 & 1.
# the main idea of how to start was given from: http://www.r-bloggers.com/clt-standard-normal-generator/
#
# arguments: n    (the number of deviates we wish created)
#            mean (the mean of the created random deviates, with a default of 0)
#            sd   (the standard deviation of the created random deviates, with a default of 1)
#
# result: a vector of n normally-distributed deviates 

my.rnorm<-function(n,mean=0,sd=1){
  
  # error trap that stops the function if the n argument is a scalar. Returns the error message "invalid arguments" 
  if (length(n)>=2)
    stop("invalid arguments")
  
  # error trap that stops the function if the n argument is less than 1. Returns the error message "invalid arguments" 
  if (n<1)
    stop("invalid arguments")
  
  # error trap that stops the function if the sd input is less than 0. Returns the error message "invalid arguments"
  if (sd<0)
    stop("invalid arguments")
  
  # error trap that stops the function if the n input is not an integer. Returns the error message "invalid arguments"
  if (n%%round(n)!=0)
    stop("invalid arguments")
  
  # creates a vector of n 0s to be filled in with the created deviates
  x<-rep(0,n)
  
  # a for loop of n times creating the random normal deviates using the runif(0,1) function
  # summing the uniform deviates then using the central limit theorem to create n standard normal deviates
  for (i in 1:n){
    u<-runif(16,0,1)
    sumu<-sum(u)
    x[i]<-((sumu-8)*sqrt(12/16))
  }
  
  # for loop of n times giving the above created standard normal deviates the mean and standard deviation
  # as indicated in the functions arguments
  for (i in 1:n){
    x[i]<-x[i]*sd+mean
  }
  
  # return of produced random normal deviates
  return(x)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: function for creating random deviates that follow a chi-square distribution
# using the standard normal deviates created by the my.rnorm function that 
# was created previously
# error traps for df are found within the my.rnorm function
#
# arguments: n    (the number of deviates we wish created)
#            df   (the degrees of freedom we want our chi-squared random deviates to have, with a default of 1)
#
# result: a vector of n chi-square distributed deviates 

my.rchisq<-function(n,df=1){
  
  # error trap that stops the function if the n argument is less than 1.
  if (n<1)
    stop("invalid arguments")
  
  # error trap that stops the function if the n input is not an integer.
  if (n%%round(n)!=0)
    stop("invalid arguments")
  
  # creates a vector of n 0s to be filled in with the created deviates
  x<-rep(0,n)
  
  # a for loop of n times creating the random chi-square deviates using the my.rnorm(df,0,1)function,
  # summing the squares of the stanard normal deviates
  for (i in 1:n){
    norm<-my.rnorm(df)
    y<-sum(norm^2)
    x[i]<-y
  }
  
  # return of produced random chi-square deviates
  return(x)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Purpose: function for creating random deviates that follow an F distribution
# using the chi-square deviates created by the my.rchisq function that 
# was created previously
# error traps for df1 and df2 are found within the my.rnorm function wich is used to create the my.rchisq function
#
# arguments: n    (the number of deviates we wish created)
#            df1  (the degrees of freedom of the numerator, with a default of 1)
#            df2  (the degrees of freedom of the denominator, with a default of 1)
#
# result: a vector of n F distributed deviates

my.rf<-function(n,df1=1,df2=1){
  
  # error trap that stops the function if the n argument is less than 1.
  if (n<1)
    stop("invalid arguments")
  
  # error trap that stops the function if the n input is not an integer.
  if (n%%round(n)!=0)
    stop("invalid arguments")
  
  # creates a vector of n 0s to be filled in with the created deviates
  f<-rep(0,n)
  
  # a for loop of n times creating the random F deviates using two my.rchisq(1,df1), my.rchisq(1,df2) functions,
  # creating two chi-square deviates with degrees of freedom stated in the functions arguments, dividing them with their
  # degrees of freedom and then dividing those to create the F-distributed deviates
  for (i in 1:n){
    u<-my.rchisq(1,df1)
    v<-my.rchisq(1,df2)
    f[i]<-(u/df1)/(v/df2)
  }
  
  # return of produced random F deviates
  return(f)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~Testing Functions~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: function for testing the my.rnom function by comparing with another Correct my.rnorm function 
# by using the set.seed comand to restart RNG (random number generator) in R.
#
# arguments: the same as in the my.rnorm function 
#            n    (the number of deviates we wish created)
#            mean (the mean of the created random deviates, with a default of 0)
#            sd   (the standard deviation of the created random deviates, with a default of 1)
#
# result: a statement indicating whether the numbers generated by the my.rnorm function
#         correspond to the numbers generated by a Correct my.rnorm function

test.my.rnorm<-function(n,mean=0,sd=1){
  
  # commands for resetting the seed of the RNG before the use the my.rnorm and 
  # correct.my.rnorm we want to compare and comands for saving as x and y the values generated
  set.seed(1)
  x<-my.rnorm(n)
  set.seed(1)
  y<-correct.my.rnorm(n)
  
  # statements if the results from the two my.rchisq functions are the same (a) or not (b) 
  a<-"the calculations used to create the deviates seem to be ok"
  b<-"something's wrong"
  
  # ifelse command thats returns statement "a" or "b" depending if the results are equal or not
  ifelse((x==y),return(a),return(b))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: function for testing the my.rchisq function by comparing with another Correct my.rchisq function by using the
# set.seed comand to restart RNG (random number generator) in R. (needs a my.rnorm and a correct.my.rchisq 
# function created from a correct.my.norm function to work)
#
# arguments: The same as in the my.rchisq function
#            n    (the number of deviates we wish created)
#            df   (the degrees of freedom we want our chi-squared random deviates to have, with a default of 1)
#
# result: a statement indicating whether the numbers generated by the my.rchisq function
#         correspond to the numbers generated by a Correct my.rchisq function

test.my.rchisq<-function(n,df=1){
  
  # commands for resetting the seed of the RNG before the use the my.rchisq and 
  # correct.my.rchisq we want to compare and comands for saving as x and y the values generated  
  set.seed(1)
  x<-my.rchisq(n)
  set.seed(1)
  y<-correct.my.rchisq(n)
  
  # statements if the results from the two my.rchisq functions are the same (a) or not (b) 
  a<-"the calculations used to create the deviates seem to be ok"
  b<-"something's wrong"
  
  # ifelse command thats returns statement "a" or "b" depending if the results are equal or not
  ifelse((x==y),return(a),return(b))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: function for testing the my.rf function by comparing with another Correct my.rf function by using the
# set.seed comand to restart RNG (random number generator) in R. (needs a my.rnorm, a my.rchisq used for my.rf and a correct.my.rf
# function created from a correct.my.rchisq created from a correct.my.rnorm function to work)
#
# arguments: the same as in the my.rf function
#            n    (the number of deviates we wish created)
#            mean (the mean of the created random deviates, with a default of 0)
#            sd   (the standard deviation of the created random deviates, with a default of 1)
#
# result: a statement indicating whether the numbers generated by the my.rf function
#         correspond to the numbers generated by a Correct my.rf function

test.my.rf<-function(n,df1=1,df2=1){
  
  # commands for resetting the seed of the RNG before the use the my.rf and 
  # correct.my.rcf we want to compare and comands for saving as x and y the values generated
  set.seed(1)
  x<-my.rf(n)
  set.seed(1)
  y<-test.rf(n)
  
  # statements if the results from the two my.rf functions are the same (a) or not (b) 
  a<-"the calculations used to create the deviates seem to be ok"
  b<-"something's wrong"
  
  # ifelse command thats returns statement "a" or "b" depending if the results are equal or not
  ifelse((x==y),return(a),return(b))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#