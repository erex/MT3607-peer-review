# I confirm that the attached is my own work, except where clearly indicated in the text.

# The function rnorm uses the central limit theorem method to generate a single 
# number drawn from a normal distribution. 

rnorm<-function(x1){          # x2 is a row vector of length x1. It's elements                                                       
  x2<-runif(x1, 0, 1)            # are numbers that have been drawn randomly from
  x3<-sum(x2)                    # a set of numbers uniformly distributed between
  (x3-8)*sqrt(12/16)            # 0 and 1.
}                                         # x3 is the sum of all of the elements in x2.

# The function v.rnorn creates a row vector of n distinct random normal variables.

v.rnorm<-function(n){        # x4 is a row vector of length n with all elements                                         
  x4<-rep(16, times=n)      # equal to 16.                 
  sapply(x4, rnorm)           # The command sapply applies the function rnorm 
}                                        # to every element in the vector x4 one at a time.
                                         # As a consequence, 16 becomes the argument, x1, 
# for rnorm. The function sappy is used to ensure that each element in v.rnorm is distinct. 
# This is due the fact that the central limit theorem method of generating a single number
# is used n distinct times.
                      
# The function m.scale will scale a row vector of random normal variables so that its mean
# and standard deviation are equal to user specified values. The output of this function
# is a column vector.

m.scale<-function(n, mean, sd){         # The scale function calculates the mean and 
  mean+sd*scale(v.rnorm(n))              # standard deviation of a vector and then to 
}                                                          # each element it subtracts the mean and divides   
                                                           # by the standard deviation.

# Once the elements of the vector have been standardised, a user specified standard deviation
# is multiplied and then a user specified mean is added. This ensures that the vector of
# random normal variables has a mean and standard deviation as specified by the user. 

# The function my.rnorm creates a row vector of n random normal variables such that the
# mean and standard deviation of the vector is equal to user specified values.

my.rnorm<-function(n, mean=0, sd=1){   # By default, if a mean and standard deviation
  errortest(n, mean, sd, y=T)                   # are not specified when using this function then 
  x5<-m.scale(n, mean, sd)                     # a row vector of n standard normal variables will
  c(x5)                                                      # be outputted.
}                                      
                                                              # my.rnorm first calls the function errortest                                       
# to make sure the user has input the correct arguments into the function. The variable 
# y tells errortest that it is the function my.rnorm it is testing. Thereafter, the 
# matrix output from m.scale, x5, is outputted as a row vector.

# The function sq squares any number that is input into it through the argument, x8.

sq<-function(x6){
  x6^2
}

# The function std.rnormsq finds the sum of n squared independent standard normal variables. 
# The number of variables being summed over defines the degrees of freedom, df.  

std.rnormsq<-function(df){                             # x7 is a natural number drawn randomly from the
  x7<-sample(2:100, 1, replace=T)                # interval [2, 100]. x8 is a row vector of      
  x8<-sample(my.rnorm(x7), df, replace=T)   # length df with elements that are independent             
  sum(sapply(x8, sq))                                     # standard normal variables. The sample function
}                                                                     # ensures that df standard normal variables are 

# drawn independently from the same distribution each time std.rnormsq is called. After each 
# element in the vector x8 has been squared, the function sum adds all elements of x8 together.

# The function my.rchisq creates a row vector of n chi squared distributed random numbers with 
# degrees of freedom df.

my.rchisq<-function(n, df=1){     # By default, if a degrees of freedom is not specified                                             
  errortest(n, df)                          # by the user then it is set equal to 1. As before, 
  x9<-rep(df, times=n)                # the function calls errortest to check to see if
  sapply(x9, std.rnormsq)           # the user has entered valid arguments into the function. 
}                                                 # x9 is a row vector of length n with elements equal to df.
                                                  # The function sapply applies the function std.rnormsq 
# to each element in the row vector x10. 

# The function f outputs a single number drawn from an F-distribution with degrees of freedom df1
# and df2.

f<-function(df1, df2){      
  x10<-my.rchisq(n=1, df1)        # x10 is the sum of df1 squared independent standard
  x11<-my.rchisq(n=1, df2)        # normal variables. Similarly, x12 is the sum of df2 
  ((x10/df1)/(x11/df2))                # squared standard normal variables. 
}

# The function my.rf creates a row vector of n random numbers drawn from an F-distribution 
# with degrees of freedom df1 and df2.

my.rf<-function(n, df1=1, df2=1){ 
  errortest(n, df1, df2)                  # x12 is a row vector of length n with elements df1. 
  x12<-rep(df1, times=n)             # Similarly, x13 is a row vector of the same length 
  x13<-rep(df2, times=n)             # but with elements df2. The function mapply applies
  mapply(f, df1=x12, df2=x13)    # the function f to the first element of each vector
}                                                 # x12 and x13, and then to the second element in each,
                                                  # and so on.                                   

# The function is.naturalnumber tests to see whether a number is a natural number or not.
# If the number inputted, n, is a natural number then the function returns TRUE, 
# otherwise it returns FALSE. 

is.naturalnumber<-function(n, tol=.Machine$double.eps^0.5){
  n>tol & abs(n-round(n))<tol
}  

# This function was created by adapting a function used to test for whole numbers, 
# is.wholenumber, found in the RStudio help files. In breif, if a number, n, is
# larger than a tiny number, tol, and the difference between n and n rounded 
# to 0 decimal places is less than tol in absolute value, then n is a natural number.

# The function errortest tests for invalid arguments entered into the functions 
# my.rnorm, my.rchisq, and my.rf by the user.

errortest<-function(n, mean=0, sd=1, 
                    df=1, df1=1, df2=1, y=F){ 
  if(missing(n))                                           # The first if-statement checks for missing values for 
    stop("Invalid arguments")                      # the argument n. This is because n is the only argument 
  if(n==1&y==T)                                         # that has no default value in all three functions. 
    stop("Invalid arguments")                      # The second if-statement checks to see if the function
  if(!is.numeric(n)|!is.numeric(mean)|         # my.rnorm has the number of values set to return, n, equal
       !is.numeric(sd)|!is.numeric(df)|           # to 1. An argument of 1 is invalid because the sample 
       !is.numeric(df1)|!is.numeric(df2))        # variance would be undefined.          
    stop("Invalid arguments")                       # The third if-statement checks if any of the arguments are  not numeric   
  if(!is.naturalnumber(n)|!is.naturalnumber(df)|      
     !is.naturalnumber(df1)|!is.naturalnumber(df2))                        
    stop("Invalid arguments")                       # The fourth if-statement checks if the number of values         
  if(is.list(n)|is.list(mean)|                            # to return in each vector, n, or the degrees of freedom, df,   
       is.list(sd)|is.list(df)|                              # df1, and df2 are natural numbers or not.    
       is.list(df1)|is.list(df2))                          # If-statements five to seven check the arguments to see if they are 
    stop("Invalid arguments")                      # incorrect data types. For instance, a mean cannot be entered as an array or data frame.      
  if(is.array(n)|is.array(mean)|                    # If-statement eight and nine check to make sure that the      
       is.array(sd)|is.array(df)|                      # degrees of freedom are not less than 1 and the 
       is.data.frame(sd)|is.data.frame(df)|    # standard deviation is not less than 0.
       is.array(df1)|is.array(df2))         
    stop("Invalid arguments")                            
  if(is.data.frame(n)|is.data.frame(mean)|                
       is.data.frame(df1)|is.data.frame(df2))  
    stop("Invalid arguments")
  if(df<1|df1<1|df2<1)
    stop("Invalid arguments")
  if(sd<0)
    stop("Invalid arguments")
}

# The function test tests to see whether the functions my.rnorm, my.rchisq, and my.rf
# produce output from the correct distribution using the defined arguments. If a 
# function passes the test, then the function outputs "Test passed", otherwise it 
# outputs "Test failed".
 
test<-function(w1){                                # First, the user enters 1, 2, or 3 into the argument of test which calls  
 n<-sample(2:100, 1)                            # either function my.rnorm, my.rchisq, or my.rf respectively for testing.
 mean<-sample(-100:100, 1)                # Thereafter, the function generates random values for all of the arguments
 sd<-sample(1:100, 1)                          # for each function.
 df1<-sample(1:100, 1)
 df2<-sample(1:100, 1)
 if(w1==1){                                            # If the user inputs 1, then the function my.rnorm is tested. In particular 
  w2<-my.rnorm(n, mean, sd)               # the function is tested to see whether it produces a row vector of the 
  w3<-(length(w2)==n&is.numeric(w2) # of the specified length, has the correct length, and the specified mean 
       &is.vector(w2)&round(mean(w2))==mean 
       &round(sd(w2))==sd)                   # and standard deviation.  
  hist(my.rnorm(n, mean, sd))              # Finally, a histogram of the variables in the vector is produced to check the distributions 
 }                                                          # shape is correct. 
 if(w1==2){                                           
  w2<-my.rchisq(n, df1)                        # In each if statement of the function, w3 comes out as either TRUE or FALSE.   
  w3<-(length(w2)==n&is.numeric(w2)# This is because w3 is the intersection, of a number of individual tests
       &is.vector(w2))                             # as outlined above. As a result, if anyone of the individual tests comes
  hist(my.rchisq(n, df1))                       # out as FALSE then w3 comes out as FALSE and the test fails.
 }                                                    
 if(w1==3){
  w2<-my.rf(n, df1, df2)
  w3<-(length(w2)==n&is.numeric(w2)
       &is.vector(w2))
  hist(my.rf(n, df1, df2))
 }
 if(w3==T){cat("Test passed", "\n")}
 else{cat("Test failed", "\n")}
}




