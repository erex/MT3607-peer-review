#1. my.rnorm function: (maximum number of input is 3 and min number of input is 1)
### Rnorm distribution which allows users to change mean and standard deviation 
### Input: n-number of values to return 
### m: population mean 
### S: population standard deviation 

my.rnorm <- function (n,m=0,s=1) {
        x<-0
        xn<-0
        value<-numeric(n)
if (is.numeric(n) == F|is.numeric(m) == F |is.numeric(s) == F ) {    ### check whether the input is numeric or not 
        print ("Invalid arguments")                                  ### remind "error"if the input is non-numeric
} else {                                                             
        if (is.na(n)==T|is.na(m) == T|is.na(s) == T ) {              ### check whether the input is NA. For example, if someone defines n <- log(-2), it's also an error
                print ("Invalid arguments")
        } else { 
                if ((n != as.integer(n))) {                          ### make sure the input N must be an integer 
                        print("invalid arguments")
                } else { 
                        if(n <=0  |s < 0) {                          ### N should be a spoitive integer and SD is shouldn't be less than 0
                                print ("invalid arguments")
                        } else {
                                for (i in 1:n) {                     ### Do calculation as the algorithm requires
                                 x<-runif(16)
                                 xn[i]<-(sum(x)-8)*sqrt(12/16)
                                 value<-m+xn*s
                                } 
                        return(value)                                 ### Return the result 
                        }                
                }     
        }   
}
}

#2. Chi-square function: (maximum number of input is 2 and min number of input is 1)
### Chi-square distribution which allows users to choose number of values to return and degree of freedom  
### Input: n-number of values to return 
### d1: Degree of freedom for m 

my.rchisq <- function(n,d=1) {
        x<-0
if (is.numeric(n) == F|is.numeric(d) == F ) {                         ### check whether the input is numeric or not
        print ("Invalid arguments")                                   ### remind "error"if the input is non-numeric
} else {
        if (is.na(n)==T|is.na(d) == T) {                              
                print ("Invalid arguments")                           ### check whether the input is NA. For example, if someone defines n <- log(-2), it's also an error
        } else { 
                if ((n != as.integer(n))|(d != as.integer(d))) {      ### make sure the input N and df must be an integer
                        print("invalid arguments")
                } else { 
                        if(n <=0 |d <= 0) {                           ### N and value of degree of freedom should be a spoitive integer
                                print ("invalid arguments")
                        } else {
                                for (i in 1:n) {
                                  x[i] <- sum(((my.rnorm(d))^2))      ### Do calculation as the algorithm requires
                                }               
                                return (x)                            ### return the result 
                                }  
                        } 
                }
       }
}


#3. F distribution: (maximum number of input is 3 and min number of input is 1)
### F distribution which allows users to choose number of values to return an two dfs 
### Input: n-number of values to return 
### d1: Degree of freedom for m 
### d2: Degree of freedom for 2


my.rf <- function(n,d1=1,d2=1) {
if (is.numeric(n) == F|is.numeric(d1) == F |is.numeric(d2) == F ) {                        ### check whether the input is numeric or not         
        print ("Invalid arguments")                                                        ### remind "error ¡°if the input is non-numeric
} else {
        if (is.na(n)==T|is.na(d1) == T|is.na(d2) == T ) {                                  ### check whether the input is NA. For example, if someone defines n <- log(-2), it's also an error
                print ("Invalid arguments")
        } else { 
             if ((n != as.integer(n))|(d1 != as.integer(d1))|(d2 != as.integer(d2))) {     ### make sure the input N,d1 and d2 must be an integer
                print("invalid arguments")
           } else { 
                if(n <=0 |d1 <= 0 |d2 <= 0) {   
                        print ("invalid arguments")                                        ### N, d1 and d2 should be positive integers
                } else {
                        for (i in 1:n) {
                                U <- sum(((my.rchisq(1,d1))^2))/d1  
                                V <- sum(((my.rchisq(1,d2))^2))/d2
                                x[i] <- U/V                                                ### Do calculation as the algorithm requires
                                }               
                        print (x)                                                          ### print result 
                        }     
                }   
        }
}
}


##### Self-test 
#Take my.rnorm() as an example of testing: 
#1.  x<- my.rnorm(n=10)
#    pass.test <-(length(x)==10 & is.numeric(x))
### the test result shows "TRUE"
#2. my.rnorm(11,5,-5)
### Result is "Invalid arguments" because the SD can't be less than 0, doesn't make sense 
#3. my.rnorm(11)
### function runs as expected 
#4. my.rnorm(10,5,3)
### function runs as expected 


#Take my.rchisq() as an example of testing:
#5. x <- log(-3), my.rchisq(x)
### Result is "Invalid arguments" because input is actually a NA
#6. my.rchisq(2,"ds")
### Result is "Invalid arguments" because input has to be a number 
#7. my.rchisq(2,3.5)
### Result is "Invalid arguments" because number of freedom should be an integer 
#8. my.rchisq(2,-3)
### Result is "Invalid arguments" because number of freedom should be a positive integer 
#9. my.rchisq(11)
### function runs as expected 
#10. my.rchisq(21,49)
### function runs as expected 


#Take my.rf() as an example of testing:
#11. x <- log(-3), my.rf(x)
### Result is "Invalid arguments" because input is actually a NA
#12. my.rf(1,-10,2.5)
### Result is "Invalid arguments" because degree of freedom should be a positive integer 
#13. my.rf(2,5,"ds")
### Result is "Invalid arguments" because all inputs here have to be numeric 
#14. x<- my.rf(n=10)
#   pass.test <-(length(x)==10 & is.numeric(x))
### The result is TRUE 
#15. my.rf(2,-3)
### Result is "Invalid arguments" because number of freedom should be a positive integer 
#16. my.rf(11)
### function runs as expected 
#17. my.rf(10,55,33)
### function runs as expected 







