#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){ 
  #Purpose: generate pseudo-random deviates from a Normal distribution
  #Inputs: 
  # n - number of pseudo-deviates requested
  # mean - the mean of the values to be returned (default value 0)
  # sd - the standard deviation of the values to be returned (default value 1)
  
  if (length(n)!=1 | length(mean)!=1 | length(sd)!=1) stop("invalid arguments") 
  #Stops the function and returns the error message "invalid arguments" if the parameters entered are not scalars 
  # e.g. if they are vectors or matrices.
  
  if (!is.numeric(n) | !is.numeric(mean) | !is.numeric(sd)) stop("invalid arguments")
  #Stopping the function if the argument is not numeric would also be a good idea, 
  # however the function already returns "invalid arguments" by default when this happens.
  
  if (n<0 | sd<0) stop("invalid arguments")
  #Stops and returns the error message if the parameters for n or sd entered are negative.
  
  if (n>10^6 | mean>10^6 | sd>10^6 ) stop("invalid arguments")
  #Stops and returns the error message if the parameters entered are greater than 10^6 
  # i.e. if they are too big.
  
  if (n==0) {
    result<-numeric(n)
  #Returns an empty vector if n was set to be 0
    
  } else {
    result<-numeric(n)
    #Creates a numeric vector called 'result' of length n. 
    #It will be used to store the standard Normal deviates generated.
    for (i in 1:n) {
      #Repeats what's in the {} for i=1,2,...,n.
    
      x<-(sum(runif(16))-8)*sqrt(12/16)
      #Applies the CLT algorithm to generate a pseudo-random standard Normal deviate from random Uniform deviates.
    
      result[i]<-x*sd+mean
      #Transforms the standard Normal deviate to a Normal deviate 
      # with the mean and sd specified when computing the function 
      # (remains standard if nothing has been specified).
      #Then stores the value in the vector 'result' at the ith position.
    }
  }
  return(result) 
  #Displays the vector 'result' i.e shows all the required Normal deviates.
}



my.rchisq<-function(n,df=1){ 
  #Purpose: generate pseudo-random deviates from a Chi-squared distribution
  #Inputs: 
  # n - number of pseudo-random deviates requested
  # df - the degrees of freedom of the distribution (default value 1)
  
  if (length(n)!=1 | length(df)!=1) stop("invalid arguments")
  #Stops and returns the error message "invalid arguments" if the parameters entered are not scalars 
  # e.g. if they are vectors or matrices.
  
  if (!is.numeric(n) | !is.numeric(df)) stop("invalid arguments")
  #Stopping the function if the argument is not numeric would also be a good idea, 
  # however the function already returns "invalid arguments" by default when this happens.
  
  if (n<0 | df<0) stop("invalid arguments")
  #Stops and returns the error message if the parameters entered are negative.
  
  if (n>10^6 | df>10^6 ) stop("invalid arguments")
  #Stops and returns the error message if the parameters entered are greater than 10^6 
  # i.e. if they are too big.
  
  if (!is.integer(df)) {df<-trunc(df)}
  #If the df entered is not an integer, only takes the integer part of the number.
  
  if (n==0) {
    result<-numeric(n)
    #Returns an empty vector if n was set to be 0
    
  } else {
    result<-numeric(n)
    #Creates a numeric vector called 'result' of length n.
  
    for (i in 1:n) {
      #Repeats what's in the {} for i=1,2,...,n.
    
      if (df==0) {
        norm<-numeric(df)
        # norm becomes an empty vector if df was set to be 0
        
      } else {
        norm<-numeric(df)
        #Creates a numeric vector called 'norm' of length df. 
        #It will be used to store the standard Normal deviates generated.
    
        for (j in 1:df) {
          #Repeats what's in the {} for j=1,2,...,df.
      
          norm[j]<-(sum(runif(16))-8)*sqrt(12/16)
          #Applies the CLT algorithm to generate a pseudo-random standard Normal deviate from random Uniform deviates.
          # then stores the value in the vector 'norm' at the jth position
        }
      }
      result[i]<-sum(norm^2)
      #Calculates the Chi squared deviate from the Normal deviates.
      #Then stores the value in the vector 'result' at the ith position.
    }
  }
  return(result) 
  #Displays the vector 'result' i.e shows all the required Chi-squared deviates.
}





my.rf<-function(n,df1=1,df2=1){ 
  #Purpose: generate random deviates from a F distribution
  #Inputs: 
  # n - number of random deviates requested
  # df1 - the degrees of freedom of the numerator (default value 1)
  # df2 - the degrees of freedom of the denominator (default value 1)
  
  if (length(n)!=1 | length(df1)!=1 | length(df2)!=1) stop("invalid arguments")
  #Stops and returns the error message "invalid arguments" if the parameters entered are not scalars 
  # e.g. if they are vectors or matrices.
  
  if (!is.numeric(n) | !is.numeric(df1) | !is.numeric(df2)) stop("invalid arguments")
  #Stopping the function if the argument is not numeric would also be a good idea, 
  # however the function already returns "invalid arguments" by default when this happens.
  
  if (n<0 | df1<=0 | df2<=0) stop("invalid arguments")
  #Stops and returns the error message if the parameters entered are negative.
  
  if (n>10^6 | df1>10^6 | df2>10^6 ) stop("invalid arguments")
  #Stops and returns the error message if the parameters entered are greater than 10^6 
  # i.e. if they are too big.
  
  if (!is.integer(df1)|!is.integer(df2)) {
    df1<-trunc(df1)
    df2<-trunc(df2)
  }
  #If the df's entered are not integers, only takes the integer part of the numbers.
  
  if (n==0) {
    result<-numeric(n)
    #Returns an empty vector if n was set to be 0
    
  } else {
    result<-numeric(n)
    #Creates a numeric vector called 'result' of length n.
    #It will be used to store the F distributed values generated.
  
    for (i in 1:n) {
      #Repeats what's in the {} for i=1,2,...,n.
    
      norm1<-numeric(df1)
      #Creates a vector called 'norm1' of length df1. 
      #It will be used to store the standard Normal deviates generated.
    
      for (j in 1:df1) {
        #Repeats what's in the {} for j=1,2,...,df1.
      
        norm1[j]<-(sum(runif(16))-8)*sqrt(12/16)
        #Applies the CLT algorithm to generate a pseudo-random standard Normal deviate from random Uniform deviates.
        #Then stores the value in the vector 'norm1' at the jth position.
      }
    
      norm2<-numeric(df2)
      for (k in 1:df2) {
        norm2[k]<-(sum(runif(16))-8)*sqrt(12/16)
        #Same as previously, this time for df2.
      }
    
      U1<-sum(norm1^2)
      U2<-sum(norm2^2)
      result[i]<-(U1/df1)/(U2/df2)
      #Calculates the F-distributed values deviate using the pseudo-random standard Normal deviates.
      #Then stores the value in the vector 'result' at the ith position.
    }
  }
  return(result) 
  #Displays the vector 'result' i.e shows all the required pseudo-random F deviates.
}



# TEST FUNCTION:
# source('asmt1.r') to be used if the test function is not in the same file as the functions to test
test.myfunctions<-function(func){
  # Purpose: run all at once tests to see 
  # - if the function returns the right output
  # - if the algorithm is well implemented 
  # - if the error traps have been set
  # Input: the function to test AS A CHARACTER
  # Output: a list of tests that will display TRUE if the test is passed and FALSE if not 
  # and two histograms displaying the student's function next to the one it is supposed to imitate.
  
  
  if (!is.character(func)) stop("Argument has to be a character e.g. 'my.rnorm', 'my.rchisq' or 'my.rf' !")
  if (length(func)!=1) stop("Argument has to be of length 1")
  # Error checks to make sure the right parameters are entered i.e. 'my.rnorm', 'my.rchisq' and 'my.rf' AS CHARACTERS
  
  test1<-switch(func,
                'my.rnorm' = try(my.rnorm(n=5),silent=T),
                'my.rchisq' = try(my.rchisq(n=5),silent=T),
                'my.rf' = try(my.rf(n=5), silent=T))
  pass.test1<-(length(test1)==5 & is.numeric(test1))
  # First test: does the function return a vector of numeric values like it is supposed to?
  # Have the default values for mean, sd, df, df1 and df2 been set?
  
  
  old.par<-par("mfrow")
  par(mfrow=c(1,2))
  on.exit(par(mfrow=old.par))
  test2<-switch(func,
                'my.rnorm' = rnorm(5000,mean=-4,sd=2),
                'my.rchisq' = rchisq(5000,1),
                'my.rf' = rf(500,45,26))
  titlea<-switch(func,
                'my.rnorm' = "rnorm(5000,mean=-4,sd=2)",
                'my.rchisq' = "rchisq(5000,1)",
                'my.rf' = "rf(500,45,26)")
  hist(test2, xlab=titlea, main="Histogram of the default R function")
  abline(v=mean(test2), col="red")
  
  pass.test2<-switch(func,
                'my.rnorm' = my.rnorm(5000,mean=-4,sd=2),
                'my.rchisq' = my.rchisq(5000,1),
                'my.rf' = my.rf(500,45,26))
  titleb<-switch(func,
                 'my.rnorm' = "my.rnorm(5000,mean=-4,sd=2)",
                 'my.rchisq' = "my.rchisq(5000,1)",
                 'my.rf' = "my.rf(500,45,26)")
  hist(pass.test2, xlab=titleb, main="Histogram of my function")
  abline(v=mean(pass.test2), col="red")
  # Second test: is the Algorithm well implemented?
  # Displays a histogram of student's function next to a histogram of the default R function
  # e.g. rchisq next to my.chisq and highlights the mean in red.
  
  
  #The rest of the tests verify if the error traps have been well implemented.
  x<-c(1:5)
  test3<-switch(func,
                'my.rnorm' = try(my.rnorm(-4,0,1), silent=T),
                'my.rchisq' = try(my.rchisq(-4,1), silent=T),
                'my.rf' = try(my.rf(-4,1,1), silent=T))
  inter3<-switch(func,
                'my.rnorm' = "Error in my.rnorm(-4, 0, 1) : invalid arguments\n",
                'my.rchisq' = "Error in my.rchisq(-4, 1) : invalid arguments\n",
                'my.rf' = "Error in my.rf(-4, 1, 1) : invalid arguments\n")
  pass.test3<-(test3==inter3)
  test4<-switch(func,
                'my.rnorm' = try(my.rnorm(n=1,mean=0,sd=-3), silent=T),
                'my.rchisq' = try(my.rchisq(n=1,df=-4), silent=T),
                'my.rf' = try(my.rf(n=1, df1=-4, df2=1), silent=T))
  inter4<-switch(func,
                'my.rnorm' = "Error in my.rnorm(n = 1, mean = 0, sd = -3) : invalid arguments\n",
                'my.rchisq' = "Error in my.rchisq(n = 1, df = -4) : invalid arguments\n",
                'my.rf' = "Error in my.rf(n = 1, df1 = -4, df2 = 1) : invalid arguments\n")
  pass.test4<-(test4==inter4)
  # Does the function detect a negative n and negative sd/df/df1?

  
  if (func == "my.rf") {
    test4.5<-try(my.rf(n=1,df1 = 1,df2=-4), silent=T)
    inter4.5<-"Error in my.rf(n = 1, df1 = 1, df2 = -4) : invalid arguments\n"
    pass.test4.5<-(test4.5==inter4.5)
  }
  # If my.rf is chosen, additional test run to check if it detects a negative df2
  
  test5<-switch(func,
                'my.rnorm' = try(my.rnorm(x,0,1), silent=T),
                'my.rchisq' = try(my.rchisq(x,1), silent=T),
                'my.rf' = try(my.rf(x,1,1), silent=T))
  inter5<-switch(func,
                'my.rnorm' = "Error in my.rnorm(x, 0, 1) : invalid arguments\n",
                'my.rchisq' = "Error in my.rchisq(x, 1) : invalid arguments\n",
                'my.rf' = "Error in my.rf(x, 1, 1) : invalid arguments\n")
  pass.test5<-(test5==inter5)
  test6<-switch(func,
                'my.rnorm' = try(my.rnorm(n=1,mean=x, sd=1), silent=T),
                'my.rchisq' = try(my.rchisq(n=1,df=x), silent=T),
                'my.rf' = try(my.rf(n=1,df1=x, df2=1), silent=T))
  inter6<-switch(func,
                'my.rnorm' = 'Error in my.rnorm(n = 1, mean = x, sd = 1) : invalid arguments\n',
                'my.rchisq' = 'Error in my.rchisq(n = 1, df = x) : invalid arguments\n',
                'my.rf' = 'Error in my.rf(n = 1, df1 = x, df2 = 1) : invalid arguments\n')
  pass.test6<-(test6==inter6)
  test7<-switch(func,
                'my.rnorm' = try(my.rnorm(n=1,mean=0,sd=x), silent=T),
                'my.rf' = try(my.rf(n=1,df2=x), silent=T))
  inter7<-switch(func,
                'my.rnorm' = 'Error in my.rnorm(n = 1, mean = 0, sd = x) : invalid arguments\n',
                'my.rf' = 'Error in my.rf(n = 1, df2 = x) : invalid arguments\n')
  pass.test7<-switch(func,
                'my.rchisq' = 'Not Applicable',
                (test7==inter7))
  # Does the function detect when a vector is entered as parameter?
  
  if (func == "my.rf") {
    result<-c(pass.test3,pass.test4,pass.test4.5,pass.test5,pass.test6,pass.test7)
    errors<-data.frame(result, row.names = c("n<0","sd/df/df1<0","df2<0","n is a vector","mean is a vector",
                                             "sd/df2 is a vector"))                                             
  } else {
    result<-c(pass.test3,pass.test4,pass.test5,pass.test6,pass.test7)
    errors<-data.frame(result, row.names = c("n<0","sd/df/df1<0","n is a vector","mean is a vector",
                                             "sd/df2 is a vector"))
  }
  return(list("Function?"=func,"Function works? Defaults set?"=pass.test1,"Algorithm good?"='If two histograms similarly distributed, then yes!',"Error traps?"=errors))
  # The results are printed in a list showing:
  #  - the test for the function
  #  - the test for the algorithm
  #  - a data frame of all the tests for the error traps
  # (The value TRUE is printed if the test is passed and FALSE if it isn't).

}

#Testing whether the function stops parameters entered from being too large (>10^6) 
# should be done manually to avoid the test function taking too much time if the 
# error message wasn't specified.

