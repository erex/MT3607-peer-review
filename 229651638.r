#I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm<-function(n,mean=0,sd=1){
  #Purpose: creates n pseudo-random normal deviates
  #Inputs: n, mean and sd - numeric scalars, with n and sd being positive 
  #Output: a vector of n pseudo-random values from a normal distribution with mean=mean and standard deviation=sd
  if (length(n)>1){
    stop ("invalid arguments") #checks n is a scalar
  } 
  if (is.numeric(n)=='FALSE'){ 
    stop ("invalid arguments") #checks n is a number
  }
  if (n<0){
    stop ("invalid arguments") #checks n is positive
  }
  if (length(mean)>1){
    stop ("invalid arguments") 
  } 
  if (is.numeric(mean)=='FALSE'){
    stop ("invalid arguments")
  }
  if (length(sd)>1){
    stop ("invalid arguments") 
  } 
  if (is.numeric(sd)=='FALSE'){
    stop ("invalid arguments")
  }
  if (sd<0) {
    stop ("invalid arguments") 
  }
  X<-numeric(n) 
  for (i in 1:n){
    a<-runif(16) 		#assigns 16 uniformly-distributed random deviates to object 'a'
    b<-(sum(a)-8)*sqrt(12/16)	#takes the 16 values in 'a' and turns them a normally-distributed deviate (which is assigned to 'b')
    c<-(b*sd)+mean 		#transforms 'b' according to the arguments of my.rnorm
    X[i]<-c 			#puts the normal deviate into the 'i'th position within 'X'
  }
  return (X)
}

my.rchisq<-function(n,df=1){
  #Purpose: creates n pseudo-random chi-squared-distributed deviates
  #Inputs: n, df - numeric positive scalars
  #Output: a vector of n pseudo-random values from a chi-squared distribution with df degrees of freedom
  if (length(n)>1){
    stop ("invalid arguments")
  }
  if (is.numeric(n)=='FALSE'){
    stop ("invalid arguments")
  }
  if (n<0){
    stop ("invalid arguments")
  }
  if (length(df)>1){
    stop ("invalid arguments")
  }
  if (is.numeric(df)=='FALSE'){
    stop ("invalid arguments")
  }
  if (df<0){
    stop ("invalid arguments")
  }
  if (df-round(df)!=0){
    stop("invalid arguments") 	#checks df is an integer
  } 
    Y<-numeric(n)
  for (i in 1:n){
    d<-my.rnorm(df) 	#assigns df random normal deviates to 'd'
    e<-sum(d^2) 	#creates a chi-squared-distributed deviate
    Y[i]<-e  		#puts the chi-squared deviate into the 'i'th position in 'Y'
  }
  return(Y)
}

my.rf<-function(n,df1=1,df2=1){
  #Purpose: creates n pseudo-random F-distributed deviates
  #Inputs: n, df1, df2 - numeric positive scalars
  #Output: a vector of n pseudo-random values distributed with an F-distribution with df1 and df2 degrees of freedom
  if (length(n)>1){
    stop ("invalid arguments")
  } 
  if (is.numeric(n)=='FALSE'){
    stop ("invalid arguments")
  }
  if (n<0){
    stop ("invalid arguments")
  }
  if (length(df1)>1){
    stop ("invalid arguments")
  }  
  if (is.numeric(df1)=='FALSE'){
    stop ("invalid arguments")
  }  
  if (df1<0){
    stop ("invalid arguments")
  }	
  if (df1-round(df1)!=0){
    stop("invalid arguments")
  }  
  if (length(df2)>1){
    stop ("invalid arguments")
  }  
  if (is.numeric(df2)=='FALSE'){
    stop ("invalid arguments")
  }
  if (df2<0){
    stop ("invalid arguments")
  }
  if (df2-round(df2)!=0){
    stop("invalid arguments")
  }
  F<-numeric(n)
  for (i in 1:n){
    U<-my.rchisq(1,df1)	#assigns one chi-squared deviate with df=df1 to 'U'
    V<-my.rchisq(1,df2)
    W<-(U/df1)/(V/df2) 	#creates one F-distributed deviate
    F[i]<-W 		#puts the F deviate into the 'i'th position in 'F'
  }
  return(F)
}

#test checks that my.rnorm produces values from a normal distibution
#counts how many times my.rnorm produces normally-distributed values, for 3:n+2 values
normality.test<-function(n){
  a<-numeric(n)
  for (i in 1:n){
    x<-my.rnorm(i+5)
    y<-shapiro.test(x)
    if (y$p.value>0.05) {a[i]=1
    }else{
       a[i]=0
    }
  }
  return(sum(a))
}

#as the normality test will sometimes fail, the following test checks that rnorm() also fails sometimes
rnormality.test<-function(n){
  a<-numeric(n)
  for (i in 1:n){
    x<-rnorm(i+2)
    y<-shapiro.test(x)
    if (y$p.value>0.05) {a[i]=1
      }else{
        a[i]=0
      }
  }
  return(sum(a))
}

#check that n, mean and sd of my.rnorm are correct
#produce a histogram of my.rnorm - to visually check the distribution
#length(my.rnorm) should always = n, so correct.n should always be TRUE
#as n increases, mean.diff and sd.diff should get closer to zero
norm.check<-function(n,mean,sd){
  dist<-my.rnorm(n,mean,sd)
  x<-mean(dist)-mean
  y<-sd(dist)-sd
  z<-length(dist)==n
  hist<-hist(dist)
  return(list(correct.n=z,mean.diff=x,sd.diff=y))
  return(hist)
}

#check that length(n) of my.rchisq = n
#check that the mean and variance of the values in the vector produced my my.rchisq are correct
#produce a histogram of my.rchisq
#mean.diff and var.diff should be small, especially for large n
#correct.n should always be TRUE
chisq.check<-function(n,df){
  w<-my.rchisq(n,df)
  x<-length(w)==n
  y<-(mean(w)-df)
  z<-(var(w)-2*df)
  hist<-hist(w)
  return(list(correct.n=x,mean.diff=y,var.diff=z))
  return(hist)
}

#check that for large df, the values produced by my.rchisq approach a normal distribution
chinorm.test<-function(n=3,df=999){
  x<-my.rchisq(n,df)
  y<-shapiro.test(x)
  if (y$p.value>0.05){return("normally distributed")
  }else{
      return("not normally distributed")
  }
}

#check that length(n) of my.rf = n
#check values created by my.rf produce correct mean and variance
#produce a histogram of my.rf
#calculate the difference between the mean of the distribution and the mean of my sample
#calculate the difference between the variance of the distribution and the variance of my sample
#mean.diff and var.diff should be small, expecially for large values of n
#correct.n should always be TRUE
f.check<-function(n,df1=3,df2=5){
  if(df2<5){
    stop("invalid arguments")
  }
  if(df1<3){
    stop("invalid arguments")
  }
  w<-my.rf(n,df1,df2)
  x<-length(w)==n
  y<-(mean(w)-(df2/(df2-2)))
  z<-(var(w)-(((2*(df2^2))*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))))
  hist<-hist(w)
  return(list(correct.n=x,mean.diff=y,var.diff=z))
  return(hist)
}

#tests to check that my.rnorm produces error messages when it gets:
#a negative n			my.rnorm(-1) 			
#a vector for n			my.rnorm(1:5)			
#a non-numeric n		my.rnorm('fred')		
#a value of NA for n		my.rnorm(NA)			
#a vector for mean		my.rnorm(10,1:5)		
#a non-numeric mean		my.rnorm(10,'fred')		
#a value of NA for mean		my.rnorm(10,NA)			
#a negative standard deviation	my.rnorm(10,5,-4)		
#a vector for sd		my.rnorm(10,5,1:5)		
#a non-numeric sd		my.rnorm(10,5,'fred')		
#a value of NA for sd		my.rnorm(10,5,NA)		

#tests to check that my.rchisq produces error messages when it gets:
#a negative n			my.rchisq(-1)			
#a vector for n			my.rchisq(1:5)			
#a non-numeric n		my.rchisq('fred')		
#a value of NA for n		my.rchisq(NA)			
#a negative df			my.rchisq(10,-1)		
#a vector for df		my.rchisq(10,1:5)		
#a non-numeric df		my.rchisq(10,'fred')		
#a value of NA for df		my.rchisq(10,NA)		
#a non-integer for df		my.rchisq(10,1.5)		

#tests to check that my.rf produces error messages when it gets:
#a negative n			my.rf(-1)			
#a vector for n			my.rf(1:5)			
#a non-numeric n		my.rf('fred')			
#a value of NA for n		my.rf(NA)			
#a negative df1			my.rf(10,-1)			
#a vector for df1		my.rf(10,1:5)			
#a non-numeric df1		my.rf(10,'fred')		
#a value of NA for df1		my.rf(10,NA)			
#a non-integer for df1		my.rf(10,1.5)			
#a negative df2			my.rf(10,10,-1)			
#a vector for df2		my.rf(10,10,1:5)		
#a non-integer for df2		my.rf(10,10,1.5)		
#a non-numeric df2		my.rf(10,10,'fred')		
#a value of NA for df2		my.rf(10,10,NA)			