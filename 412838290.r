#I confirm that the attached is my own work, except where clearly indicated in the text.

# Function 1
# the function my.rnorm is used to create n normal distribted variables with a chosen value for standard deviation and a chosean value for the mean
# The function is based on the algorithm for normally distributed variables following the central limit theorem method included in the assignment (Rubinstein 1981:89-90)

my.rnorm <- function(n, variable.mean=0, variable.sd=1)
  # variable.mean is the mean value for the generated normal distribution, a default value of 0 is used
  # variable.sd is the required standard deviation for the generated normal distribution, the default value used is 1
  # n defines the number of values for the normal distribution to be generated, o Default value is given here
{ 
  # Take input and check if the inputs n/variable.mean/variable.sd are numeric values, if not, then stop the function and print "invalid argument" as well as reason for failure
    if (is.numeric(n) == FALSE| is.numeric(variable.mean) == FALSE| is.numeric(variable.sd) == FALSE)
    stop (cat("invalid argument - variable is not numeric"))
    
    # Take input and check if the inputs n/variable.mean/variable.sd are vectors containing more than 1 number, if not, then stop the function and print "invalid argument" as well as reason for failure
    if (length(n) >= 2 | length(variable.mean) >= 2| length (variable.sd) >= 2)
      stop (cat("invalid argument - variable is not scalar"))
 
  # Take input and check if input n < 1, if not then stop function and print "invalid argument" as well as reason for failure
  if( n <1|variable.sd<0) 
    stop (cat("invalid argument - entered variable is not of sufficient value"))
  # Take input and check if input n is an integer, if not then stop function and print "invalid argument" as well as reason for failure
  if (n%%round(n)!= 0)
    stop (cat("invalid argument - variable is not integer"))
  
  # Overview: Design a random normally-distributed variable (called xmeansd) and repeat the process up to n times
  
  # This creates an column or "vector" of n zeros.
  x<-rep(0,n)
  
  #loop the function over i times for 1 to n
  for(i in 1:n) 
  {
    #create a uniform distributed variable u where i runs from 1 to n
    u<-runif(n, 0, 1)               
    
    #Take the sum of u and assign the function sum(u) to the name sumofu
    sumofu<-sum(u)                  
    
    #assign x[i] using a formula to create normal distribution applying the central limit theorem
    x[i]<-(sumofu-(n/2))*sqrt(12/n) 
  }
  #transform normal distribution by changing mean and standard deviation
  variable.meansd <- x * variable.sd + variable.mean  
  
  #return normal distributed variable "variable.meansd"
  return (variable.meansd)                            
}

#Test1 Function 1
x<-my.rnorm(n=15)
pass.test<-(length(x)==15 & is.numeric(x))
# pass.test takes on the value TRUE, or 1 if 'x' contains 10 numeric values 


#Function 2
# the function my.rchisq is used to create n chi square distribted variables with a chosen value for the degrees of freedom
# The function is based on the algorithm for chi squared distributed variables given in the assignment (Larsen and Marx 1981:284)
my.rchisq <- function(n, variable.df=1){
  
  # Take input and check if the inputs n/df are numeric values, if not, then stop the function and print "invalid argument" as well as reason for failure
  if (is.numeric(n) == FALSE| is.numeric(variable.df) == FALSE)
    stop (cat("invalid argument - variable is not numeric"))
  
  # Take input and check if input n/df1 < 1, if not then stop function and print "invalid argument" as well as reason for failure
  if( n <1|variable.df<1) 
    stop (cat("invalid argument - entered variable is not of sufficient value"))
  
  # Take input and check if input n is an integer, if not then stop function and print "invalid argument" as well as reason for failure
  if(n%%round(n)!=0 | variable.df%%round(variable.df)!=0 )
    stop(cat("invalid argument - variable is not an integer"))
  
  # Take input and check if the inputs n/df are vectors containing more than 1 number, if not, then stop the function and print "invalid argument" as well as reason for failure
  if (length(n) >= 2 | length(variable.df) >= 2)
    stop (cat("invalid argument - variable is not scalar"))
  
  # Overview: Design a chi square distributed variable (xchi) and repeat the process up to n times
  
  # This creates an column or "vector" of n zeros.
  x.chi<-rep(0,n)
  
  #loop the function over i times for 1 to n
  for(i in 1:n){
    # Take n random, normally distributed variable created in Function 1 whereas n=df and assign the function to the name chinorm
    chinorm<-my.rnorm(variable.df)
    # Take the sum of chinorm, square this and assign it to the variable y
    y<-sum(chinorm^2)
  }
  # assign xchi[i] using a formula to create chi squared distributions
  x.chi[i]<-y
  ##return chi squared distributed variable "xchi"
  return(x.chi)
}


#Test1 Function 2
x<-my.rchisq (n=15)
pass.test<-(length(x)==15 & is.numeric(x))
# pass.test takes on the value TRUE, or 1 if 'x' contains 10 numeric values 

#Function 3
# the function my.rchisq is used to create F-distribted variables with two chosen value for the degrees of freedom
# The function is based on the algorithm foollowing the distribution theory theorem for F-distributed variables given in the assignment (Larsen and Marx 1981:293))
my.rf<-function(n, variable.df1=1, variable.df2=1){
  
  # Take input and check if input n/df!/df2 < 1, if not then stop function and print "invalid argument" as well as reason for failure
  if( n <1 | variable.df1<1 | variable.df2<1) 
    stop (cat("invalid argument - entered value is not sufficient"))
  
  # Take input and check if input n is an integer, if not then stop function and print "invalid argument" as well as reason for failure
  if(n%%round(n)!=0 | variable.df1%%round(variable.df1)!=0 | variable.df2%%round(variable.df2)!=0)
  stop("invalid argument - value is not an integer")
  
  # Take input and check if the inputs n/df1/df2 are numeric values, if not, then stop the function and print "invalid argument" as well as reason for failure
  if (is.numeric(n) == FALSE| is.numeric(variable.df1) == FALSE | is.numeric(variable.df2) == FALSE)
    stop (cat("invalid argument - variable is not numeric"))
  
  # Take input and check if the inputs n/df1/df2 are vectors containing more than 1 number, if not, then stop the function and print "invalid argument" as well as reason for failure
  if (length(n) >= 2 | length(variable.df1) >= 2 | length(variable.df2) >= 2)
    stop (cat("invalid argument - variable is not scalar"))
  
  #This creates an column or "vector" of n zeros.
  x.f<-rep(0,n)
  
  #loop the function over i times for 1 to n
  for(i in 1:n){
    
    # Take n random, normally distributed variable created in Function 1 whereas n=df1 and assign the function to the name norm.df1
    norm.df1<-my.rnorm(variable.df1)
    # Take n random, normally distributed variable created in Function 1 whereas n=df2 and assign the function to the name norm.df2
    norm.df2<-my.rnorm(variable.df2)
    
    # Take the sum  assign it to the variable y
    y<-(sum(norm.df1^2)*variable.df2)/(sum(norm.df2^2)*variable.df1)
    
    #assign x.f[i] using a formula to create F-distributions
    x.f[i]<-y
  }
  # return F-distributed variable "x.f"
  return(x.f)
}

#Test1 Function 3
x<-my.rf (n=15)
pass.test<-(length(x)==15 & is.numeric(x))
# pass.test takes on the value TRUE, or 1 if 'x' contains 10 numeric values 
