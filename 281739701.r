
#-------------------------------------------------#

#-------------------------------------------------#

#Function1 

#Purpose: This function is to generate pseudo-random values from a normal distribution
#In general: I created a function, which is almost a copy of an already existing function "rnorm"
#Arguments:   n - number of values to return
#             mean  - mean of values to return (default=0)
#             sd  - standard deviation of values to return (default=1)

#I have specified the three arguments I wanted to have in my function.
my.rnorm<-function(n, mean=0, sd=1){ 
  
  #This error trap says that if someone wants to run this function with specified argument "n", which is not numeric, it will stop the function and an error message saying "invalid arguments" will appear.  
  if (is.numeric(n)==FALSE)     
    stop ("invalid arguments")
  
  #This error trap says that if someone wants to run this function with specified argument "mean", which is not numeric, it will stop the function and an error message saying "invalid arguments" will appear.
  if (is.numeric(mean)==FALSE)  
    stop ("invalid arguments")
  
  #This error trap says that if someone wants to run this function with specified argument "sd", which is not numeric, it will stop the function and an error message saying "invalid arguments" will appear.
  if (is.numeric(sd)==FALSE)    
    stop ("invalid arguments")
  
  #This error trap says that if someone wants to run this function with specified agument n<1 ("n" below 1), it will stop the function and there will be an error message saying "invalid arguments".
  if (n<1)                      
    stop ("invalid arguments") 
  
  #This error trap says that if someone wants to run this function with specified agument sd<0 ("sd" below 0), it will stop the function and there will be an error message saying "invalid arguments".
  if(sd<0)                      
    stop("invalid arguments") 
  
  #This error trap says that if someone wants to run this function with n that is decimal, it will stop the function and there will be an error message saying "invalid arguments".
  if(n%%round(n)!=0)            
    stop("invalid arguments")
  
  #I replicate the values in x.
  x<- rep(0,n)                  
  
  #I create a loop which will work n times within this curly backet. At first, I produce 16 random values from the uniform (16 is just an example). Then I count the algorithm for normally-distributed deviates n times.
  for(i in 1:n){                
    unif<-runif(16,0,1)  
    sumu<-sum(unif)
    x[i]<-(sumu-(8))*sqrt(12/16)
  }
  
  #I create a loop which will work n times. It changes the algorythm by myltiplying by "sd" and adding "mean" value if specified.
  for (i in 1:n){               
    x[i]<-x[i]*sd+mean
  }
  
  #The function returns the calculated values of "x". 
  return(x)                     
} 

#-------------------------------------------------#

#FUnction2

#Purpose: This function is to generate pseudo-random chi^2-distributed deviates
#In general: I created a function, which is almost a copy of an already existing function "rchisq".
#Arguments:   n - number of values to return
#             df - degrees of freedom of the distribution

#I have specified the two arguments I wanted to have in my function. One of them has a default values; for argument "df", the default value equals 1.
my.rchisq<-function(n,df=1){  
  
  #This error trap says that if someone wants to run this function with specified agument n<1 ("n" below 1), it will stop the function and there will be an error message saying "invalid arguments".
  if(n<1)                     
    stop("invalid arguments")
  
  # This error trap says that if someone wants to run this function with n that is decimal, it will stop the function and there will be an error message saying "invalid arguments". (How do I achieve it? I take given "n" and devide it by the rounded n and if it is not 0, the function will not work)
  if(n%%round(n)!=0)          
    stop("invalid arguments")
  
  #I replicate the values in x. 
  x<-rep(0,n)                 
  
  #I create a loop which will work n times. I create normally-distributed deviates df times. Using the values, I count the algorithm for Chi-Squared deviates.
  for(i in 1:n){              
    norm<-my.rnorm(df)
    y<-sum(norm^2)
    x[i]<-y
  }
  
  #The function returns the calculated values of "x".
  return(x)                   
}

#-------------------------------------------------#

#FUnction3
#Purpose: This function is to generate pseudo-randomm F-distributed deviates
#In general: I created a function, which is almost a copy of an already existing function "rf"
#Arguments:   n - number of values to return
#             df1 - degrees of freedom of the numerator
#             df2 - degrees of freedom of the denominator

#I have specified the three arguments I wanted to have in my function. Two of them have the default values; for argument "df1" the default value equals 1, for argument "df2" the default value equals 1 as well.
my.rf<-function(n,df1=1,df2=1){   

  #This error trap says that if someone wants to run this function with specified agument n<1 ("n" below 1), it will stop the function and there will be an error message saying "invalid arguments".
  if(n<1)                         
    stop("invalid arguments")
  
  # This error trap says that if someone wants to run this function with n that is decimal, it will stop the function and there will be an error message saying "invalid arguments". (How do I achieve it? I take given "n" and devide it by the rounded n and if it is not 0, the function will not work)
  if(n%%round(n)!=0)              
    stop("invalid arguments")
  
  #I replicate the values in x.
  x<-rep(0,n)                     
  
  #I create a loop which will work n times. I create normally-distributed deviates df1 times and df2 times. Using the values, I count the algorithm for F-distributed deviates.
  for(i in 1:n){                  
    norm1<-my.rnorm(df1)
    norm2<-my.rnorm(df2)
    y<-(sum(norm1^2)*df2)/(sum(norm2^2)*df1)
    x[i]<-y
  }
  
  #The function returns the calculated values of "x".
  return(x)                       
}

#-------------------------------------------------#

#Additional Functions
#Purpose: The functions below are to test the randomness of the values generated with the my.rnorm, my.rchisq and my.rf functions.
#In general: Two random vectors that consist of random values should not be equal. There is a possibility that one or two of the values in the two vectors will be the same, but for sure it would not happen to all of the values.
#Arguments: n - number of values to return

#for my.rnorm

#I have specified one argument ("n"), which says how many values will there be in each vector.
pass.test1<-function(n){            

  #I create two different vectors which consist of n random values. I substract one vector by the other. I put the criteria that if the result from the substraction equals 0, then the output = "False", otherwise the output = "True".
  check1<-my.rnorm(n)             
  check2<-my.rnorm(n)
  spr<-check1-check2
  x<-ifelse(spr==0,"False","True")
  
  #The function returns the calculated values of "x", "True" if the numbers prove to be random (meaning the numbers in two vectors are not the same), otherwise "False".
  return(x)                         
}

#-------------------------------------------------#
#for my.rchisq

#I have specified one argument ("n"), which says how many values will there be in each vector.
pass.test2<-function(n){            
 
  #I create two different vectors which consist of n random values (generated by my.rchisq function). I substract one vector by the other. I put the criteria that if the result from the substraction equals 0, then the output = "False", otherwise the output = "True".
  check1<-my.rchisq(n)              
  check2<-my.rchisq(n)
  spr<-check1-check2
  x<-ifelse(spr==0,"False","True")
  
  #The function returns the calculated values of "x", "True" if the numbers prove to be random (meaning the numbers in two vectors are not the same), otherwise "False".
  return(x)                         
}

#-------------------------------------------------#

#for my.rf

#I have specified one argument ("n"), which says how many values will there be in each vector.
pass.test3<-function(n){            
 
  #I create two different vectors which consist of n random values. I substract one vector by the other. I put the criteria that if the result from the substraction equals 0, then the output = "False", otherwise the output = "True".
  check1<-my.rf(n)                  
  check2<-my.rf(n)
  spr<-check1-check2
  x<-ifelse(spr==0,"False","True")
  
  #The function returns the calculated values of "x", "True" if the numbers prove to be random (meaning the numbers in two vectors are not the same), otherwise "False".  
  return(x)                         
}

#-------------------------------------------------#

#results of the testing:
pass.test1(10)
pass.test2(10)
pass.test3(10)