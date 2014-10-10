#MT3607 Assignment 1
#I confirm that the attached is my own work, except where clearly indicated in the text
#Note:My testing function is my.Test() which uses the other functions

my.CheckValid<-function(n,allowNeg,allowDec){
  #Purpose:To check a given input satisfies the required properties for the other functions
  #Input:n-any variable
  #      allowNeg-boolean, true if the variable can be negative or 0
  #      allowDec-boolean, true if the variable can be a real number
  #Output:TRUE/FALSE if the variable is valid or not
  #Note:Checks the input variable against a series of conditions and if it fails any, returns false
  #For these procedures, no element needs to be postive and 0 but not negative, so negative and 0 will be
  #treated together
  if(!(is.numeric(n))){                   #Tests if the variable is not a numeric data type 
    return(FALSE)
  }
  if(length(n)>1){                        #Tests if the variable is a vector or list type
    return(FALSE)
  }
  if((n-round(n))!=0 & allowDec==FALSE){  #Tests if it is not a whole number
    return(FALSE)
  }
  if(n<=0 & allowNeg==FALSE){             #Tests if it is a negative number or 0
    return(FALSE)
  }
  return(TRUE)
}

#------------------------------------------------------------------------------#

my.rnorm<-function(n,mean=0,sd=1){
  #Purpose:Generate a random normal variable by the central limit theorem
  #Input:n-positive integer, the number of variable to be produced
  #      mean-real number parameterising the normal distribution
  #      sd-positive real number parameterising the normal distribution
  if(my.CheckValid(n,FALSE,FALSE)==FALSE){  #Test that n is a positive integer
    stop("Invalid Argument")
  }
  if(my.CheckValid(mean,TRUE,TRUE)==FALSE){ #Test that mean is a real number
    stop("Invalid Argument")
  }
  if(my.CheckValid(sd,FALSE,TRUE)==FALSE){  #Test that sd is a positive real number
    stop("Invalid Argument")
  }
  x <- vector(mode="numeric", length=n)   #Define a vector to return the random variables 
  for(counter1 in 1:n){       #For each random variable
    u<-0
    for(counter2 in 1:16){    #Sum 16 random uniform variables
      u<-u+runif(1)
    }
    u<-(u-8)*(sqrt(12/16))  #Scale to the central limit
    x[counter1]<-(u*sd)+mean
  }   #Convert to the required parameters of the desired normal distribution
  return(x)
}

#------------------------------------------------------------------------------#

my.rchisq<-function(n,df=1){
  #Purpose:Generate a random chi-squared variable using the my.rnorm function
  #Input:n-positive integer, the number of variable to be produced
  #      df-positive integer, the degrees of freedom of the variable
  if(my.CheckValid(n,FALSE,FALSE)==FALSE){    #Test that n is a positive integer 
    stop("Invalid Argument")
  }
  if(my.CheckValid(df,FALSE,FALSE)==FALSE){   #Test that df is a positive integer
    stop("Invalid Argument")
  }
  c <- vector(mode="numeric", length=n) #Define a vector to return the variables
  for(counter1 in 1:n){     #For each random variable
    x<-my.rnorm(df)         #Create the correct number of standard normal random variables
    for(counter2 in 1:df){  #Square them all
      x[counter2]<-(x[counter2])^2
    }
    c[counter1]<-sum(x)   #Calculate the final chi-squared variable
  }    
  return(c)
}

#------------------------------------------------------------------------------#

my.rf<-function(n,df1=1,df2=1){
  #Purpose:Generate a random f-distribution variable using my.rchisq
  #Input:n-positive integer, the number of variable to be produced
  #      df1-positive integer, parameterising the distribution
  #      df2-positive integer, parameterising the distribution  
  if(my.CheckValid(n,FALSE,FALSE)==FALSE){    #Check n is a positive integer
    stop("Invalid Argument")
  }
  if(my.CheckValid(df1,FALSE,FALSE)==FALSE){  #Check df1 is a positive integer
    stop("Invalid Argument")
  }
  if(my.CheckValid(df2,FALSE,FALSE)==FALSE){  #Check df2 is a positive integer
    stop("Invalid Argument")
  }
  f <- vector(mode="numeric", length=n)      #Define a vector to return the random variables 
  for(counter1 in 1:n){         #Repeat for each random variable
    u<-my.rchisq(1,df1)
    v<-my.rchisq(1,df2)
    f[counter1]<-(u/df1)/(v/df2)  #Calculate the random f variable with the 2 chi-squared rvs
  }
  return(f) 
}

#------------------------------------------------------------------------------#

my.GenerateTest<-function(){
  #Purpose:To create a suitable random selection of test input for the functions
  #Input:n/a
  #Output:A 4x8 matrix of numerical test input
  #Note:This produces 4 different types of numerical data(Positive integer,Negative integer,
  #     Positive real and Negative real) with 8 of each type ranging in scale from 10^-4 to 
  #     10^4
  test <- matrix(1:32,nrow=4,ncol=8) #Declare the matrix variable
  for(i in 1:8){
    test[1,i]<-(runif(1))*10^(i-4)              #Generate a random positive real in scale
    test[2,i]<--1*(runif(1))*10^(i-4)           #Generate a random negative real in scale
    test[3,i]<-round((runif(1))*10^(i-4))+1     #Generate a random positive integer in scale
    test[4,i]<--1*round((runif(1))*10^(i-4))-1  #Generate a random negative integer in scale
  }
  return(test)
}

#------------------------------------------------------------------------------#

my.ErrorHandle<-function(f){
  #Purpose:To determine if a function runs or produces an error message
  #Input:n/a
  #Output:Boolean(True/False),TRUE if an error occurs and FALSE if not
  #Note:This function was adapted from the lecture notes from lecture 5 of MT3607
  res<-try(f,silent=TRUE)
  if((class(res)[1]=="try-error")){   #If the function produces an error
    return(TRUE)
  }else{      #If the function runs as desired
    return(FALSE)
  }
}

#------------------------------------------------------------------------------#

test.rnorm<-function(){
  #Purpose:Test the function my.rnorm
  #Input: n/a
  #Output: numErrors(The number of unexpected errors or results)
  #Note:Tests the function my.rnorm using a range of different inputs and if any 
  #        unexpected results or errors occur, then they are recorded
  
  #Generate the data to be used in the test
  invalid<-c("CHAR",TRUE)   #Invalid data of character and boolean type
  test <- my.GenerateTest() #Numerical data from ??10^-4 to ??10^4, with integers and real numbers 
                            #Into a 4*8 matrix as explained in my.GenerateTest()
  numErrors<-0              #Initialise the variable to count the number of errors
  
  #Test the argument "n"
  #n should be a positive integer, and reject all other input
  for(i in 1:8){ #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rnorm(test[1,i],0,1))){  #Positive real should be rejected
      numErrors<-numErrors+1
    }  
    if(!my.ErrorHandle(my.rnorm(test[2,i],0,1))){  #Negative real should be rejected
      numErrors<-numErrors+1
    }  
    if(my.ErrorHandle(my.rnorm(test[3,i],0,1))){   #Positive integer should be accepted
      numErrors<-numErrors+1
    }   
    if(!my.ErrorHandle(my.rnorm(test[4,i],0,1))){  #Negative integer should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rnorm(0,0,1))){  #0 should be rejected
    numErrors<-numErrors+1
  }  
  for(i in 1:2){
    if(!my.ErrorHandle(my.rnorm(invalid[i],0,1))){  #Character and Boolean data should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rnorm(invalid,0,1))){ #List/Vector data should be rejected
    numErrors<-numErrors+1
  } 
  
  #Test the argument "mean"
  #"mean" can be any real number so should reject non-numerical input
  #"mean" has default 0, so argument can also be left blank
  if(my.ErrorHandle(my.rnorm(1,sd=1))){  #Default=0, so no error
    numErrors<-numErrors+1
  }
  for(i in 1:8){  #Test for the differing orders of magnitude
    if(my.ErrorHandle(my.rnorm(1,test[1,i],1))){  #Positive real should be accepted
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rnorm(1,test[2,i],1))){  #Negative real should be accepted
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rnorm(1,test[3,i],1))){  #Positive integer should be accepted
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rnorm(1,test[4,i],1))){  #Negative integer should be accepted
      numErrors<-numErrors+1
    }
  }
  if(my.ErrorHandle(my.rnorm(1,0,1))){  #0 should be accepted
    numErrors<-numErrors+1
  }
  for(i in 1:2){  #Non-numerical data should not be accepted
    if(!my.ErrorHandle(my.rnorm(1,invalid[i],1))){
      numErrors<-numErrors+1
    }
  } 
  if(!my.ErrorHandle(my.rnorm(1,invalid,1))){  #Vector/List data should be rejected
    numErrors<-numErrors+1
  }
  
  #Test the argument "sd"
  #"sd" can be any positive real number so should reject non-numerical input, negative numbers and 0
  #"sd" has default 1, so argument can also be left blank
  if(my.ErrorHandle(my.rnorm(1,mean=1))){  #Default=1, so no error
   numErrors<-numErrors+1
  }
  for(i in 1:8){ #Test for the differing orders of magnitude
    if(my.ErrorHandle(my.rnorm(1,0,test[1,i]))){  #Positive real should be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rnorm(1,0,test[2,i]))){ #Negative real should be rejected
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rnorm(1,0,test[3,i]))){  #Positive integer should be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rnorm(1,0,test[4,i]))){ #Negative integer should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rnorm(1,0,0))){     #0 should be rejected
    numErrors<-numErrors+1
  }
  for(i in 1:2){
    if(!my.ErrorHandle(my.rnorm(1,0,invalid[i]))){  #Character and boolean data should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rnorm(1,0,invalid))){  #Vector/List data should be rejected
    numErrors<-numErrors+1
  }
  return(numErrors)
}

#------------------------------------------------------------------------------#

test.rchisq<-function(){
  #Purpose:Test the function my.rchisq
  #Input: n/a
  #Output: numErrors(The number of unexpected errors or results)
  #Note:Tests the function my.rchisq using a range of different inputs and if any 
  #        unexpected results or errors occur, then they are recorded
  
  #Generate the data to be used in the test
  invalid<-c("CHAR",TRUE)   #Invalid data of character and boolean type
  test <- my.GenerateTest() #Numerical data from ??10^-4 to ??10^4, with integers and real numbers 
                            #Into a 4*8 matrix as explained in my.GenerateTest()
  numErrors<-0              #Initialise the variable to count the number of errors
  
  #Test the argument "n"
  #n should be a positive integer, and reject all other input
  for(i in 1:8){  #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rchisq(test[1,i],1))){  #Positive real should be rejected
      numErrors<-numErrors+1
    }  
    if(!my.ErrorHandle(my.rchisq(test[2,i],1))){  #Negative real should be rejected
      numErrors<-numErrors+1
    }  
    if(my.ErrorHandle(my.rchisq(test[3,i],1))){   #Positive integer should be accepted
      numErrors<-numErrors+1
    }   
    if(!my.ErrorHandle(my.rchisq(test[4,i],1))){  #Negative integer should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rchis(0,1))){  #0 should be rejected
    numErrors<-numErrors+1
  }  
  for(i in 1:2){
    if(!my.ErrorHandle(my.rchisq(invalid[i],1))){  #Character and Boolean data should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rchisq(invalid,1))){ #List/Vector data should be rejected
    numErrors<-numErrors+1
  } 

  #Test the argument "df"
  #degrees of freedom can be only positive integers so should reject all negative and 0 input
  #"df" has default 1, so argument can also be left blank
  if(my.ErrorHandle(my.rchisq(1))){  #Default=1, so no error
    numErrors<-numErrors+1
  }
  for(i in 1:8){  #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rchisq(1,test[1,i]))){  #Positive real should not be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rchisq(1,test[2,i]))){  #Negative real should not be accepted
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rchisq(1,test[3,i]))){  #Positive integer should be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rchisq(1,test[4,i]))){  #Negative integer should not be accepted
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rchisq(1,0))){  #0 should not be accepted
    cat("help")
    numErrors<-numErrors+1
  }
  for(i in 1:2){  #Non-numerical data should not be accepted
    if(!my.ErrorHandle(my.rchisq(1,invalid[i]))){
      numErrors<-numErrors+1
    }
  } 
  if(!my.ErrorHandle(my.rchisq(1,invalid))){  #Vector/List data should be rejected
    numErrors<-numErrors+1
  }

  return(numErrors)
}

#------------------------------------------------------------------------------#

test.rf<-function(){
  #Purpose:Test the function my.rf
  #Input: n/a
  #Output: numErrors(The number of unexpected errors or results)
  #Note:Tests the function my.rf using a range of different inputs and if any 
  #        unexpected results or errors occur, then they are recorded
  
  #Generate the data to be used in the test
  invalid<-c("CHAR",TRUE)   #Invalid data of character and boolean type
  test <- my.GenerateTest() #Numerical data from ??10^-4 to ??10^4, with integers and real numbers 
  #Into a 4*8 matrix as explained in my.GenerateTest()
  numErrors<-0              #Initialise the variable to count the number of errors
  
  #Test the argument "n"
  #"n" should be a positive integer, and reject all other input
  for(i in 1:8){ #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rf(test[1,i],1,1))){  #Positive real should be rejected
      numErrors<-numErrors+1
    }  
    if(!my.ErrorHandle(my.rf(test[2,i],1,1))){  #Negative real should be rejected
      numErrors<-numErrors+1
    }  
    if(my.ErrorHandle(my.rf(test[3,i],1,1))){   #Positive integer should be accepted
      numErrors<-numErrors+1
    }   
    if(!my.ErrorHandle(my.rf(test[4,i],1,1))){  #Negative integer should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rf(0,1,1))){  #0 should be rejected
    numErrors<-numErrors+1
  }  
  for(i in 1:2){
    if(!my.ErrorHandle(my.rf(invalid[i],1,1))){  #Character and Boolean data should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rf(invalid,1,1))){ #List/Vector data should be rejected
    numErrors<-numErrors+1
  } 

  #Test the argument "df1"
  #"df1" should be a positive integer, and reject all other input
  #"df1" has default 1, so argument can also be left blank
  if(my.ErrorHandle(my.rf(1,df2=1))){  #Default=1, so no error
    numErrors<-numErrors+1
  }
  for(i in 1:8){  #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rf(1,test[1,i],1))){  #Positive real should be rejected
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rf(1,test[2,i],1))){  #Negative real should not be accepted
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rf(1,test[3,i],1))){  #Positive integer should be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(!my.rf(1,test[4,i],1))){  #Negative integer should not be accepted
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(!my.rf(1,0,1))){  #0 should be rejected
    numErrors<-numErrors+1
  }
  for(i in 1:2){  #Non-numerical data should not be accepted
    if(!my.ErrorHandle(my.rf(1,invalid[i],1))){
      numErrors<-numErrors+1
    }
  } 
  if(!my.ErrorHandle(my.rf(1,invalid,1))){  #Vector/List data should be rejected
    numErrors<-numErrors+1
  }

  #Test the argument "df2"
  #"df2" should be a positive integer, and reject all other input
  #"df2" has default 1, so argument can also be left blank
  if(my.ErrorHandle(my.rf(1,df1=1))){  #Default=1, so no error
    numErrors<-numErrors+1
  }
  for(i in 1:8){ #Test for the differing orders of magnitude
    if(!my.ErrorHandle(my.rf(1,1,test[1,i]))){  #Positive real should be rejected
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rf(1,1,test[2,i]))){ #Negative real should be rejected
      numErrors<-numErrors+1
    }
    if(my.ErrorHandle(my.rf(1,1,test[3,i]))){  #Positive integer should be accepted
      numErrors<-numErrors+1
    }
    if(!my.ErrorHandle(my.rf(1,1,test[4,i]))){ #Negative integer should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rf(1,1,0))){     #0 should be rejected
    numErrors<-numErrors+1
  }
  for(i in 1:2){
    if(!my.ErrorHandle(my.rf(1,1,invalid[i]))){  #Character and boolean data should be rejected
      numErrors<-numErrors+1
    }
  }
  if(!my.ErrorHandle(my.rf(1,1,invalid))){  #Vector/List data should be rejected
    numErrors<-numErrors+1
  }
  return(numErrors)
}

#------------------------------------------------------------------------------#

my.Test<-function(){
  #Purpose:To test all 3 random variable generators an provide feedback
  #Input: n/a
  #Output:Will collect the number of errors with each function and print out to the user
  
  x<-test.rnorm()    #Test the my.rnorm function and provide user feedback on it
  cat("The function my.rnorm has ",x," unexpected errors\n")
  x<-test.rchisq()  #Test the my.rchisq function and provide user feedback on it
  cat("The function my.rchisq has ",x," unexpected errors\n")
  x<-test.rf()      #Test the my.rf function and provide user feedback on it
  cat("The function my.rf has ",x," unexpected errors\n")
}