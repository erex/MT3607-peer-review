#I confirm that the attached is my own work, except where clearly indicated in the text.



my.rnorm<-function(n,mean=0,sd=1) {
    
    #Purpose: To generate n pseudo-random normal distributed deviates
    #Inputs: 
    #   n - number of values to return(no default)
    #   mean - mean of values to return(default 0)
    #   sd - standard deviation of values to return (default 1)
    #Outputs: Returns a vector, of length n, of pseudo-random values from a normal distribution
    
    #Error checking on inputs
    #checks inputs are of type numeric
    if (!is.numeric(n)|!(is.numeric(mean))|!(is.numeric(sd))) {
        return("invalid arguments")
    }
    #checks n is a scalar 
    if (length(n)>1) {
        return("invalid arguments")
    } else {
        #checks n is not negative and is an integer
        if ((n<0)|!(n%%1==0)) {
            return("invalid arguments")
        } else {
            if (sd<0) {
                return("invalid arguments")
            }
        }
    }
    
    
    #deals with special case of when n=0
    if (n==0) {
        return(numeric(0)) 
    } else {
        #generates a vector of length n
        X<-c(1:n)
        
        #generates n values from a standard normal distribution 
        for (j in 1:n) {
            #creates one random standard normal value from random uniform values
            k<-seq(from=1,to=16,by=1)
            for (i in 1:length(k)) {
                k[i]<-runif(1,0,1)
            }
            X[j]<-((sum(k[1:length(k)]))-8)*(sqrt(12/16))
        }
    }
    
    #transforms vector of standard normal values into a vector of normal values with mean and sd given in function 
    Answer<-(X*sd)+mean
    
    #returns Answer(vector of n normal pseudo-random values)
    return(Answer)
}





my.rchisq<-function(n,df=1) {
    #Purpose: To generate n pseudo-random chi-square distributed variates
    #Inputs: 
        #n - number of values to return (no default)
        #df - degrees of freedom (default==1)
    #Outputs: Returns a vector, of length n, of pseudo-random values from a chi-square distribution
    
    #Error checking on inputs
        #checks n and df are of type numeric
        if (!is.numeric(n)|!(is.numeric(df))) {
            return("invalid arguments")
        }
    
    #checks on input: n
        #checks n in not negative and is an integer
        if ((n<0)|!(n%%1==0)) {
            return("invalid arguments")
        } else {
            #special case when n==0
            if (n==0) {
                return(numeric(0)) 
            } else {
                #Assigns a sequence of length n (if n>0) to vaiable X
                X<-seq(1:n)
            }
        }
    #checks on input: df
        #checks df is not negative and is of type integer
        if ((df<0)|!(df%%1==0)) {
            return("invalid arguments")
        } else {
            #special case when df==0
            if (df==0) {
                return(rep(0,n))
            } else {
                #assigns a sequence of length df to vaiable L
                if(df>=1) {
                    L<-seq(1:df)
                }
            }
        }
    
    
    #generates one pseudo-random chi-squared distributed value from previous function, my.rnorm
    for (i in 1:n) {
        for (j in 1:df) {
            L[j]<-(my.rnorm(1,0,1))^2
        }
        X[i]<-sum(L)    
    }
    
    #returns a vector of n pseudo-random chi-square distributed values 
    Answer<-X
    return(Answer)
}






my.rf<-function(n,df1=1,df2=1) {
    #Purpose: To generate n pseudo-random F-distributed deviates
    #Inputs:
    #n -  number of values to return(no default)
    #df1 - degrees of freedom of the numerator(default 1)
    #df2 - degrees of freedom of the denominator(default 1)
    #Outputs: Returns a vector, of length n, of pseudo-random values from an F-distribution
    
    
    #Error checking on inputs
    #checks inputs are of type numeric
    if (!is.numeric(n)|!(is.numeric(df1))|!(is.numeric(df2))) {
        return("invalid arguments")
    }
    #checks n is not negative and is an integer 
    if ((n<0)|!(n%%1==0)) {
        return("invalid arguments")
    } else {
        #checks that df1 and df2 are non-negative 
        if ((df1<=0)|(df2<=0)) {
            return("invalid arguments")
        } else {
            #checks df1 and df2 are integers
            if ((!(df1%%1==0))|(!(df2%%1==0))) {
                return("invalid arguments")
            } 
        }
    }
    
    #special case when n==0
    if (n==0) {
        return("numeric(0)")
    }
    
    
    #generates two vector of n psuedo-random chi-square values; one with df1 and one with df2  
    U<-my.rchisq(n,df1)
    V<-my.rchisq(n,df2)
    
    #creates vector of n psuedo-random F-distibuted variates
    F<-(U/df1)/(V/df2)
    
    #returns a vector of n psuedo-random F-distributed variates
    Answer<-F    
    return(Answer)
}




#Tests for functions


#Test 1
ntestnormal<-function(n,FUN=my.rnorm,FUN2=rnorm) {
    
    #Purpose: checks if output for function 'my.rnorm' is numeric and checks the input n and displays a logical statement if the 
    #length of the output for 'my.rnorm' and it's equivalent 'rnorm' are the same.
    
    #Inputs: 
    #n - number of output values for functions ( FUN1 and FUN2) 
    #FUN1 - function 'my.rnorm'
    #FUN2 - function 'rnorm'
    
    #Outputs: Two statements (1)indicating whether the output for the function 'my.rnorm' is numeric or not and, 
    #(2)indicating whether the output for length of the function is the same as it's equivalent function. ie. 'length(my.rnorm(n))' and 'length(rnorm(n))' 
    
    #checks if output of function 'my.rnorm' is numeric or not
    A<-FUN(n)
    if (is.numeric(A)) {
        A
        cat("Output for function, my.rnorm, is numeric\n")
    } else {
        cat("Output for function, my.rnorm, is not numeric\n")
    }
    
    
    #Error check if n is not a scalar or is not an integer (ie.invalid arguments)
    if ((length(n)<1)|(length(n)>1)|(!(n%%1==0))) {
        cat("n is not scalar or an integer\n")     
    } else {
        
        #checks if both funtions give the same length for their outputs
        #Special case: n==0
        if (n==0) {
            n
            answer1<-length(FUN(n))
            answer2<-length(FUN2(n))
            answer3<-answer1==answer2
            cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
        } else {
            #case: n positive integer
            if ((n%%1==0) & (n>0)) {
                n
                answer1<-length(FUN(n))
                answer2<-length(FUN2(n))
                answer3<-length(FUN(n)) & length(FUN2(n))
                cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
            } else {
                if (n<0) {
                    n
                    cat("n=",n,",n is negative\n")
                }
            }
        }
    }
}




#Test 2
ntestchisquare<-function(n,FUN=my.rchisq,FUN2=rchisq,df=1) {
    
    #Purpose: checks if output for function 'my.rchisq' is numeric and checks the input n and displays a logical statement if the 
    #length of the output for 'my.rchisq' and it's equivalent 'rchisq' are the same.
    
    #Inputs: 
    #n - number of output values for functions ( FUN1 and FUN2) 
    #FUN1 - function 'my.rchisq'
    #FUN2 - function 'rchisq'
    #df - degrees of freedom
    
    #Outputs: Two statements (1)indicating whether the output for the function 'my.chisq' is numeric or not and, 
    #(2)indicating whether the output for length of the function is the same as it's equivalent function. ie. 'length(my.rchisq(n))' and 'length(rchisq(n))' 
    
    #checks if output of function 'my.rchisq' is numeric or not
    A<-FUN(n)
    if (is.numeric(A)) {
        A
        cat("Output for function, my.rchisq, is numeric\n")
    } else {
        cat("Output for function, my.rchisq, is not numeric\n")
    }
    
    
    #Error check if n is not a scalar or is not an integer (ie.invalid arguments)
    if ((length(n)<1)|(length(n)>1)|(!(n%%1==0))) {
        cat("n is not scalar or an integer\n")     
    } else {
        
        #checks if both funtions give the same length for their outputs
        #Special case: n==0
        if (n==0) {
            n
            answer1<-length(FUN(n,df))
            answer2<-length(FUN2(n,df))
            answer3<-answer1==answer2
            cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
        } else {
            #case: n positive integer
            if ((n%%1==0) & (n>0)) {
                n
                answer1<-length(FUN(n,df))
                answer2<-length(FUN2(n,df))
                answer3<-answer1==answer2
                cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
            } else {
                if (n<0) {
                    n
                    cat("n=",n,",n is negative\n")
                }
            }
        }
    }
}





#Test 3
ntestF<-function(n,FUN=my.rf,FUN2=rf,df1=1,df2=1) {
    
    #Purpose: checks if output for function 'my.rf' is numeric and checks the input n and displays a logical statement if the 
    #length of the output for 'my.rf' and it's equivalent 'rf' are the same.
    
    #Inputs: 
        #n - number of output values for functions ( FUN1 and FUN2) 
        #FUN1 - function 'my.rf'
        #FUN2 - function 'rf'
        #df1 - degrees of freedom for numerator (ie.for FUN1)
        #df12 - degrees of freedom for denominator (ie.for FUN2)
    #Outputs: Two statements (1)indicating whether the output for the function 'my.rf' is numeric or not and, 
    #(2)indicating whether the output for length of the function is the same as it's equivalent function. ie. 'length(my.rf(n))' and 'length(rf(n))' 
    
    #checks if output of function 'my.rf' is numeric or not
    A<-FUN(n)
    if (is.numeric(A)) {
        A
        cat("Output for function, my.rf, is numeric\n")
    } else {
        cat("Output for function, my.rf, is not numeric\n")
    }
    
    #Error check if n is not a scalar or is not an integer (ie.invalid arguments)
    if ((length(n)<1)|(length(n)>1)|(!(n%%1==0))) {
        cat("n is not scalar or an integer\n")     
    } else {
        
        #checks if both funtions give the same length for their outputs
        #Special case: n==0
        if (n==0) {
            n
            answer1<-length(FUN(n,df))
            answer2<-length(FUN2(n,df))
            answer3<-answer1==answer2
            cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
        } else {
            #case: n positive integer
            if ((n%%1==0) & (n>0)) {
                n
                answer1<-length(FUN(n,df1,df2))
                answer2<-length(FUN2(n,df1,df2))
                answer3<-answer1==answer2
                cat("n=",n,"Function gives length",answer1,"Equivalent the same?",answer3,"\n")
            } else {
                if (n<0) {
                    n
                    cat("n=",n,",n is negative\n")
                }
            }
        }
    }
}