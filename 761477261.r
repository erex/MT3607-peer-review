#I confirm that the attached is my own work, except where clearly indicated in the text.
#my.rnorm function
#input:n,mean,sd with defaults mean=0 & sd=1
#creates n normal variables from uniform distribution
my.rnorm<-function(n,mean=0,sd=1){
  
  #check that creating more than 0 numbers, if not halt execution and print mesage "invalid arguments"
  if(n<=0){
    b<-TRUE
    stop("invalid arguments")
  }
  
  #check that n, mean & sd are all numeric values, if theyre not halt execution & print message
  if((class(n)!="numeric")|(class(mean)!="numeric") | (class(sd)!="numeric")){
    stop("invalid arguments")
  }
  
  #check that n is an integer (i.e. if you subtract the integer value from the original value there should be no decimal places) otherwise halt execution
  if(n-as.integer(n)!=0){
    stop("invalid arguments")
  }
  
  #check that n,mean,sd are all scalars, SCALAR:anything of length>1, otherwise halt & print message
  if((length(n)>1)|(length(mean)>1)|(length(sd) >1)){
    stop("invalid arguments")
  }
  
  #assign y,j as objects of type numeric with the length as 16 and n respectively
  y<-numeric(16)
  j<-numeric(n)
  
  #loop for each n value to be created
  for (i in 1:n ){
    #generate 16 random uniform values and store in object y
    for (v in 1:16){
      y[v] <- runif(1)
     
    }
    
    #add all the values in y subtract 8 and multiply by sqrt(12/16) for each i (where i loops for the number of n to be created)
    j[i]<- (sum(y)-8)*sqrt(12/16)
    #transform each value of j, by the standard deviation and mean
    j[i]<-(j[i]*sd)+mean
   
  }
  #output: return a value for j for each n
  return(j)
}

#my.rchisq function
#input: n, df where default is df=1
#creates a vector of pseudo-random chi sqr distributed deviates
my.rchisq<-function(n, df=1){
  
  #check that generating more than 0 deviates and that the degree of freedom > 0
  #if it doesnt, halt execution and print "invalid arguments"
  if ((n <= 0)|(df<=0)){
    stop("invalid arguments") 
  } 
  
  #check that objects n & df are of type numeric
  #if any are not, halt execution and print error message
  if((class(n)!="numeric")|(class(df)!="numeric")){
    stop("invalid arguments")
  }
  
  #check that objects n & df are scalars, SCALAR: object of length 1
  #if they are not, halt execution and print error message
  if (length(n) > 1 | length(df) >1){
    stop("invalid arguments")
  }
  
  #check that n & df are integer numbers, (i.e. if you subtract the integer value from the original value there should be no decimal places)
  #if not halt execution & print error message
  if((n-as.integer(n)!=0) | (df-as.integer(df)!=0) ){
    stop("invalid arguments*")
  }
  
  #assign objects f & f2 as objects of type numeric with length df &n respectively
  f<-numeric(df)
  f2<-numeric(n)
  
  #loop for each value of n to be created
  for (i in 1:n ){
      #use my.rnorm function above to create df, normal random variables
      f <- my.rnorm(df)  
      #assign for each position i in object f2, the sum of all the values in f squared
      f2[i]<-sum(f^2)
  }
  
        
  #check that each chi-sqr variable created in each position of f2 is greater than 0
  if(f2[i] <=0){
      stop("invalid arguments")
  }  
  #output: return the vector f2 of chi-sqr distributed deviates
  return(f2)
    
}

#my.rf function
#input: n, df1, df2 where default is df1=1 & df2=1
#returns a vector of pseudo-random F distributed deviates
my.rf<-function(n,df1=1,df2=1){
  
  #check that number of deviates to create >0 and that both degrees of freedom are >0
  #if not halt execution & print error message
  if ((n <= 0)|(df1<=0)|(df2<=0)){
    stop("invalid arguments") 
  } 
  
  #check that all of n,df1,df2 are of type numeric
  #if not halt execution & print error message
  if((class(n)!="numeric")|(class(df1)!="numeric") | (class(df2)!="numeric")){
    stop("invalid arguments")
  }
  
  #check that all n,df1,df2 are scalars, SCALAR: objects of length=1
  #if not, halt execution and print error emssage
  if ((length(n) > 1) | (length(df1) >1) | (length(df2) >1)){
    stop("invalid arguments")
  }
  
  #check that n,df1,df2 are all integer numbers (i.e. if you subtract the integer value from the original value there should be no decimal places)
  #if not halt execution and print error message
  if((n-as.integer(n)!=0)| (df1-as.integer(df1)!=0)| (df2-as.integer(df2)!=0)){
    stop("invalid arguments")
  }
  
  #assign U,V,ff objects of type numeric with lengths = 1,1,n respectively
  U<-numeric(1)
  V<-numeric(1)
  ff<-numeric(n)
  
  #loop for number n to be created
  for (i in 1:n){
    
    #use my.rchisq function to create 1 chi-sqr distributed deviates with degrees of freedom = df1
    U<-my.rchisq(1,df1)
    #use my.rchisq function to create 1 chi-sqr distributed deviates with degrees of freedom = df2
    V<-my.rchisq(1,df2)
    
    #each position of object ff is an f distributed deviate using above values
    ff[i]<-((U/df1)/(V/df2))
    
    #check that neither of the chi-sqr distributed deviates U&V are less than 0
    #if they are, halt execution and print out "invalid arguments"
    if((V<0)|(U<0)){
        stop("invalid arguments")
    }
  }
  #output: return vector of f-distributed deviates ff
  return(ff)
    
}

#test.my.rnorm function
#input:n,mean,sd
#tests the rnorm function
test.my.rnorm<-function(n,mean=0,sd=1){
  
  #call the rnorm function and assign results to object a
  a<-my.rnorm(n,mean,sd)
  
  #check that the number of elements being outputted is correct otherwise halt execution and print message
  if(length(a)==n){
    cat("The correct number of elements are outputted")
  }
  
  #check that the numbers outputted are of numeric type
  for(i in 1:n){
    #if any of the objects in each position of a are not numeric then
    if(class(a[i])!="numeric"){
      #halt execution, output "output is not numeric"
      stop("Output is not numeric")
    }
  }
  
  #check that the numbers outputted are scalars
  for(i in 1:n){
    #if the length of any of the objects in vector a are not equal to 1 then
    if(length(class(a[i]))!=1){
      #halt execution and output words in the string
      stop("Output is not a scalar")
    }
  }
}

#test.my.rchisq function
#input:n,df where default is df=1
#tests the function my.rchisq()
test.my.rchisq<-function(n,df=1){
  
  #call function my.rchisq and store results in vector p
  p<-my.rchisq(n,df) 
  
  #check the correct number of elements are outputed otherwise halt execution and print message  
  if(length(p)==n){
    cat("The correct number of elements are outputted")
  }
  
  #check that the numbers outputted are numbers otherwise halt execution and print message 
  for(i in 1:n){
    if(class(p[i])!="numeric"){
      stop("Output is not numeric")
    }
  }
  
  #check that the numbers outputted are scalars otherwise halt execution and print message
  for(i in 1:n){
    if(length(class(p[i]))!=1){
      stop("Output is not a scalar")
    }
    
  }
  
  #check that each chi-sqr is >=0 otherwise halt execution and print message
  #for each object outputted
  for(i in 1:n){
    #if any of the objects have value <0 then
    if(p[i]<0){
      #halt execution & output which value is <0
      stop("Output, ", i, " is less than 0.")
    }
  }
}


#test.my.rf function
#input: n,df1,df2 with defaults being df1=1 & df2=1
#tests the my.rf function
test.my.rf<-function(n,df1=1,df2=1){
  
  w<-my.rf(n,df1,df2) 
  
  #check the correct number of elements are outputed otherwise halt execution and print message  
  if(length(w)==n){
    cat("The correct number of elements are outputted")
  }
  
  #check that the numbers outputted are numbers otherwise halt execution and print message
  for(i in 1:n){
    if(class(w[i])!="numeric"){
      stop("Output is not numeric")
    }
    
  }
  #check that the numbers outputted are scalars otherwise halt execution and print message
  for(i in 1:n){
    if(length(class(w[i]))!=1){
      stop("Output is not a scalar")
    }
    
  }
  
  #check that each f value is >=0 otherwise halt execution and print message
  for(i in 1:n){
    if(w[i]<0){
      stop("Output, ", i, " is less than 0.")
    }
  }
  
  
}

