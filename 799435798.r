#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){
#Purpose: 
#Returns n pseudo-random variables from a normal distribution
  
#Inputs: 
#n- number of observations: a numeric scalar, 
#mean - mean: a numeric scalar with default 0, 
#sd- standard deviation: a numeric scalar with default 1
  
#Outputs: 
#a vector of n pseudo-random values from a normal distribution
  
  #Stops the function and returns an error message if the arguments are invalid
  if(is.numeric(n) & is.numeric(mean) & is.numeric(sd) ){
  }else{
    stop("invalid arguments")
  }
  if(sd < 0) stop("invalid arguments")
  if(n <= 0) stop("invalid arguments")
  if(n!=round(n)) stop("invalid arguments")
  
  #Vector to contain the normally distributed deviates
  normdev<-c(rep(0,n))
  
  #for loop using the central limit method to find n normally distributed deviates
  for(i in 1:n){
    U<-runif(16,0,1)
    x<-((sum(U)-8)*sqrt(12/16))*sd+mean
    normdev[i]<-x
  }
  return(normdev)
}


my.rchisq<-function(n,df=1){
#Purpose: 
#Returns n pseudo-random chi-squared distributed deviates
  
#Inputs: 
#n- number of observations: a numeric scalar
#df-degrees of freedom: a numeric scalar with default 1
  
#Outputs: 
#a vector of n pseudo-random chi-squared distributed deviates
  
  #Stops the function and returns an error message if the arguments are invalid
  if(is.numeric(n) & is.numeric(df)){
  }else{
    stop("invalid arguments")
  }
  if(df <= 0) stop("invalid arguments")
  if(n <=0 ) stop("invalid arguments")
  if(n!=round(n)) stop("invalid arguments")
  if(df!=round(df)) stop("invalid arguments")  
  
  #Create vector for the pseudo-random chi-squared distributed deviates
  chisqdev<-c(rep(0,n))

  #for loop to calculate the chi-squared distributed deviates by summing 
  #the squares of normal random variables 
  for(i in 1:n){
    z<-my.rnorm(df)
    chi<-sum(z^2)
    chisqdev[i]<-chi
  }  
  return(chisqdev)
}


my.rf<-function(n,df1=1,df2=1){
  
#Purpose: 
#Returns n pseudo-random F-distributed deviates
  
#Inputs: 
#n- number of observations: a numeric scalar
#df1- degrees of freedom: a numeric scalar with default 1
#df2- degrees of freedom: a numeric scalar with default 1
  
#Outputs: 
#a vector of n pseudo-random F-distributed deviates
  
  #Stops the function and returns an error message if the arguments are invalid
  if(is.numeric(n) & is.numeric(df1) & is.numeric(df2) ){
  }else{
    stop("invalid arguments")
  }
  if(df1 <= 0) stop("invalid arguments")
  if(df2 <= 0) stop("invalid arguments")
  if(n <= 0) stop("invalid arguments")
  if(n!=round(n)) stop("invalid arguments")
  if(df1!=round(df1)) stop("invalid arguments")
  if(df2!=round(df2)) stop("invalid arguments")
  
  #Vector to contain the f distributed deviates
  ftestdev<-c(rep(0,n))
  
  #For loop to calcualte the n f-distributed deviates
  for(i in 1:n){
    u<-my.rchisq(1,df1)
    v<-my.rchisq(1,df2)
    f<-(u/df1)/(v/df2)
    ftestdev[i]<-f
  }
  
  return(ftestdev)
}







test.myrnorm<-function(n,mean=0,sd=1){
#Purpose:
#Test to check that my.rnorm recognises invalid inputted arguments
#and consequently doesn't produce a vector of incorrect numeric values
#(checks my.rnorm gives an error when a negative n or sd is inputted and 
#when n isn't a whole number).
  
#Inputs:
#(Same as my.rnorm)
#n- number of observations: a numeric scalar, 
#mean - mean: a numeric scalar with default 0, 
#sd- standard deviation: a numeric scalar with default 1
  
#Outputs:
#Pass or Fail
  
  x<-try(my.rnorm(n,mean,sd),silent=TRUE)
  if(((sd<0) | (n<=0) | (n!=round(n))) & is.numeric(x)){
    cat("Fail")
  }else{
    cat("Pass") 
  }  
}



test.myrchisq<-function(n,df=1){
#Purpose:
#Test to check that my.rchisq recognises invalid inputted arguments 
#and consequently doesn't produce a vector of incorrect values
#(checks my.rchisq gives an error when a negative n or df is inputted and 
#when n or df aren't a whole number)
  
#Inputs:
#(Same as my.rchisq)
##n- number of observations: a numeric scalar
#df-degrees of freedom: a numeric scalar with default 1
  
#Outputs:
#Pass or Fail  
  
  x<-try(my.rchisq(n,df),silent=TRUE)
  if(((n!=round(n)) | (n<=0) | (df!=round(df)) | (df<=0)) & is.numeric(x)){
    cat("Fail")
    
  }else{
    cat("Pass")
  }
}



test.myrf<-function(n,df1=1,df2=1){
#Purpose:
#Test to check that my.rf recognises invalid inputted arguments 
#and consequently doesn't produce a vector of incorrect values
#(checks my.rf gives an error when a negative n,df1 or df2 is inputted and 
#when n, df1 or df2 isn't a whole number)
  
#Inputs:
#(Same as my.rf)
#n- number of observations: a numeric scalar
#df1- degrees of freedom: a numeric scalar with default 1
#df2- degrees of freedom: a numeric scalar with default 1
  
#Outputs:
#Pass or Fail
  
  x<-try(my.rf(n,df1,df2),silent=TRUE)
  if(((n!=round(n))|(n<=0)|df1!=round(df1)|(df1<=0)|df2!=round(df2)|(df2<=0)) & is.numeric(x)){             
    cat("Fail")
  }else{
    cat("Pass")
  }
}



outputtest<-function(n,mean=0,sd=1,df=1,df1=1,df2=1,test){
#Purpose:
#Test to check that the output of the function (when it hasn't given an error)
#is the correct amount of values and that is numeric

#Inputs:
#To test the function my.rnorm: test=1
#To test the function my.rchisq: test=2
#To test the function my.rf: test=3
#n- number of observations: a numeric scalar, 
#mean - mean: a numeric scalar with default 0 (argument only used when test=1)
#sd- standard deviation: a numeric scalar with default 1 (argument only used when test=1)
#df-degrees of freedom: a numeric scalar with default 1 (argument only used when test=2)
#df1- degrees of freedom: a numeric scalar with default 1 (argument only used when test=3)
#df2- degrees of freedom: a numeric scalar with default 1 (argument only used when test=3)

#Outputs:
#Pass or Fail
  
  switch(as.character(test),
         "1"= (x<-my.rnorm(n,mean,sd)),
         "2"= (x<-my.rchisq(n,df)),
         "3"= (x<-my.rf(n,df1,df2)))
  
  if(length(x)==n & is.numeric(x)){
    cat("Pass")
  }else{
    cat("Fail")
  }
}


