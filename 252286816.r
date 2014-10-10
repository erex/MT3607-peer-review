#I confirm that the attached is my own work, except where clearly indicated in the text.

# Normal RV generator
# purpose: my.rnorm is a random number generator to simulate random normal variables. 
#          Simulated by the uniform RV command "runif" then transformed by an algorithm into Normal random variables.
# inputs:  n - number of values to return. positive numeric integer. default = 1
#          mean - mean of returned values.  numeric.  default = 0 
#          sd - standard deviation of returned values. positive numeric.  default = 1.
# outputs: n random normal variables with mean "mean" and standard deviation "sd"
#          "invalid arguments" if n, mean or sd arguments are not acceptable forms.


my.rnorm <- function(n,mean=0,sd=1){       
  if (sd >= 0 && n==as.integer(n) && n >=0 && is.numeric(mean)==T && is.numeric(sd)==T && is.numeric(n)==T){        #check if entered arguments are valid
    c <- vector(length = n)        #c stores the n random normal variables
    for (i in 1:n){        #loop for each normal RV to be simulated      
      c[i]<-(((sum(runif(16))-8)*((12/16)^0.5))*sd)+mean        #performs the algorithmic transformation from uniform RV to Normal RV
    }        
    return(c)        #returns n normal RVs
  } else {        # if the entered arguments were not valid        
    stop("invalid arguments")        # an error message stating invalid arguments is returned        
  }
}




# Chi-Squared RV generator
# Purpose: my.rchisq function simulates random chi-squared variables. 
#          Chi-squared random variables are simulated by summing squared my.rnorm generated standard normal variables.
# Inputs:  n - number of values to return.  Must be positive numeric integer.  No Default
#          df - degrees of freedom ie: how many squared normal variables are summed.  Positive numeric integer.  Default = 1
# Outputs: n chi-squared random variables with df degrees of freedom
#          "invalid arguments" if n or df are not acceptable forms.    

my.rchisq <- function( n, df=1){        
  if (n==as.integer(n) && df==as.integer(df) && n >=0 && df >=0 && is.numeric(n)==T && is.numeric(df)==T){        # checks if entered arguments are valid
    c <- vector(length = n)        #create a vector in which to store the n random chi-squared variables
    for (i in 1:n){        #loop for each Chi-squared RV to be simulated
      c[i] <- sum((my.rnorm(n = df, sd = 1, mean = 0))^2)        #sums the squared normal RVs
    }
    return(c)        #returns n chi-squared RVs
  } else {        # if the entered arguments were not valid  
    stop("invalid arguments")        # an error message stating invalid arguments is returned   
  }
}





#F-Distribution RV generator
# Purpose: my.rf function simulates random F-Distribution variables.  
#          F-Distribution RVs simulated by simulating 2 chi-squared RVs, dividing by their degrees of freedom, then dividing one by the other
# Inputs:  n - number of values to return.  Must be positive numeric integer.  No Default
#          df1 -  numerator degrees of freedom ie: no of squared normal RVs are summed in the chi-squared RV.  Positive numeric integer.  Default = 1
#          df2 - denominator degrees of freedom ie: no of squared normal RVs are summed in the Chi-Squared RV.  Positive numeric integer.  Default = 1
# Outputs: n F-Distribution RVs with df1 and df2 degrees of freedom
#          "invalid arguments" if n, df1 or df2 are not acceptable forms.  

my.rf <- function(n, df1=1, df2=1){      
  if (n==as.integer(n) && df1==as.integer(df1) && df2==as.integer(df2) && n >= 0 && df1 >= 0 && df2 >= 0 && is.numeric(n)==T && is.numeric(df1)==T && is.numeric(df2)==T){        #checks if entered arguments are valid
    c <- vector(length=n)       #c stores the n random F-Distribution variables
    for (i in 1:length(c)){        #loop for n F-Distribution RVs
      c[i] <- (((my.rchisq(1,df1))/df1)/((my.rchisq(1,df2))/df2))       #Transforms by the algorithm the Chi-Squared RVs into F-Distribution RVs 
    }
    return(c)        #returns n F-Distribution RVs
  } else {        # if the entered arguments were not valid  
    stop("invalid arguments")        # an error message stating invalid arguments is returned
  }
}




#Test 1
#purpose: Ask the functions for a random number of RVs then check that number of RVs are returned and if they are numeric.
#inputs:  none
#output:  "pass test" if the correct number of numeric values are returned
#         "fail test" if the incorrect number of values are returned or values are not numeric

test.1 <- function(){        
  a <- sample(1:100,1)        #assigns a a random integer between 1 and 100
  b <- sample(1:100,1)        #assigns b a random integer between 1 and 100
  c <- sample(1:100,1)        #assigns c a random integer between 1 and 100
  t <- c(my.rnorm(a),my.rchisq(b),my.rf(c))        #concatenates RVs from each function into a vector t.
  if (length(t)==(a+b+c) && t==as.numeric(t)){       # verifies that t contains the correct number of RVs and they are numeric
    return("pass test")        # returns a success
  } else {        #if not numeric or incorrect numer of RVs
    return("fail test")        #return a failure
  }
}




#Test 2
# purpose: Creates a distribution of 5000 random variables from the my.rnorm function.  Should be the Normal Distibution N(0.1)
#          Uses a shapiro-wilks test to perform a hypothesis test on the distibution being Normal.
#          Null hypothesis H0:if the distribution is normal. Alternative hypothesis H1: the distribution is not normal
#inputs: none
#outputs: "Accept H0: Distribution is Normal" if p-value > 0.05
#         "Reject H0 in favour of H1:Distribution is not Normal" if p-value < 0.05

test.2 <- function(){
        reject.H0 <- shapiro.test(my.rnorm(5000))        
                #implimentation note - reject.H0 is a list of 4 results from shapiro-wilk test, the 2nd of which is the p-value
        if ((reject.H0[2]<=0.05)==F){
          return("Accept H0: Distribution is Normal")
        } else {
          return("Reject H0 in favour of H1:Distribution is not Normal")
        }       
}