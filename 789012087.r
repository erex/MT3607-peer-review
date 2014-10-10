#~~~~~~~~~~~~~~~~MT3607 Assignment1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#I confirm that the attached is my own work, except where clearly indicated in the text.


#~~~~~~~~~~~~~~~~~my.rnorm Function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

my.rnorm<-function(n,mean=0,sd=1){
  #write function that return a vector of random normal values
  
  #return a vector of random normal values
  #input n ,number of values to return,no default
  #input mean ,mean of values to return, default o 
  #input sd, sd of values to return,default 1
  z<-numeric(n)
  x<-numeric(n)
  
  #inputs for n should be numeric and positive integer values,else stop
  #inputs for mean should be numeric values,else stop
  #inputs for sd should be numeric and non-negative values,else stop
  if(is.numeric(n)==F){
    stop("invalid arguments: please input numeric value for n",call. = FALSE)
  } else if(!n%%1==0|n<=0){
    stop("invalid arguments: please input positive integer number for n ")
  } else if(!is.numeric(mean)){
    stop("invalid arguments: please input numeric value for mean")
  } else if(!is.numeric(sd)){
    stop("invalid arguments: please input numeric value for sd")
  } else if(sd<0) {
    stop("invalid argument: please input non-negative number for sd")
  }
  
  for(i in 1:n){
    z[i]<-(sum(runif(16))-8)*(12/16)^0.5
    x[i]<-sd*z[i]+mean
  }
  return(x)
}

#~~~~~~~~~~~~~my.rchisq Function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

my.rchisq<-function(n,df=1){
  #function that creates a vector of random values from chisq distribution
  
  #input n:number of values to return,no default
  #input df:degree of freedom of the distribution,default 1
  
  if(!is.numeric(n)) stop("invalid arguments: please enter numeric values for n",call.=FALSE)
  if(n<=0) stop("invalid arguments: please enter positive number for n",call. = FALSE)
  if(!n%%1==0) stop("invalid arguments: please enter integer number for n",call. = FALSE)
  if(!is.numeric(df)) stop("invalid arguments: please enter numeric values for df",call.=FALSE)
  if(df<=0) stop("invalid arguments: please enter positive number for df",call. = FALSE)
  if(!df%%1==0) stop("invalid arguments: please enter integer number for df",call. = FALSE)
  
  
  chisq<-numeric(n)
  for(i in 1:n){
    z<-my.rnorm(df,0,1)
    z2<-z^2
    chisq[i]<-sum(z2)
  }
  return(chisq)
}


#~~~~~~~~~~~~~~~~~~~~~~my.rf Function~~~~~~~~~~~~~~~~~~~~~~~~~~#

my.rf<-function(n,df1=1,df2=1){
  #function that generates a vector of random values with an F-distribution
  
  #input n:number of values to return,no default
  #input df1:degree of freedom of the numerator,default 1
  #input df2:degree of freedom of the denominator,default 1
  
  if(!is.numeric(n)) stop("invalid arguments: please enter numeric values for n",call.=FALSE)
  if(n<=0) stop("invalid arguments: please enter positive number for n",call. = FALSE)
  if(!n%%1==0) stop("invalid arguments: please enter integer number for n",call. = FALSE)
  if(!is.numeric(df1)) stop("invalid arguments: please enter numeric values for df1",call.=FALSE)
  if(df1<=0) stop("invalid arguments: please enter positive number for df1",call. = FALSE)
  if(!df1%%1==0) stop("invalid arguments: please enter integer number for df1",call. = FALSE)
  if(!is.numeric(df2)) stop("invalid arguments: please enter numeric values for df2",call.=FALSE)
  if(df1<=0) stop("invalid arguments: please enter positive number for df2",call. = FALSE)
  if(!df1%%1==0) stop("invalid arguments: please enter integer number for df2",call. = FALSE)
  
  
  for(i in 1:n){
    u<-my.rchisq(1,df1)
    v<-my.rchisq(1,df2)
    F[i]<-(u/df1)/(v/df2)
  }
  
  return(F)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~test functions~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~test function for my.rnom~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

test.my.rnorm<-function(n,mean=0,sd=1){
  
  #Purpose: test my.rnorm function. Draws 2 plots of sampled data returned by my.rnorm
  # and returns a data summary (which is printed by default).
  #Inputs:
  # n- a numeric vector;mean;sd
  #Outputs:
  # a summary object
  x<-my.rnorm(n,mean=0,sd=1)
  pass.test<-(length(x)==n & is.numeric(x))
  if(pass.test==T) print('length of generated is correct and all values are numeric')
  
  # set the output device so it shows two plots on one page
  #back up first and restore at the end
  old.par<-par("mfrow")
  par(mfrow=c(2,2))
  on.exit(par(mfrow=old.par))
  
  #plot histogram and a density line from my.rnorm
  hist(x, probability=TRUE,xlab="values from my.rnorm",main="Histogram of my.rnorm")
  lines(density(x),col="blue")
  
  # normal probability plot for my.rnorm
  qqnorm(x,main="Normal Q-Q Plot for my.rnorm",ylab="my.rnorm Quantiles",col="red")
  
  #plot histogram and a densiy line from rnorm
  y<-rnorm(n,mean=0,sd=1)
  hist(y, probability=TRUE,xlab="values from rnorm",main="Histogram of rnorm")
  lines(density(x),col="blue")
  
  #normal probability plot for rnorm
  qqnorm(y,main="Normal Q-Q Plot for rnorm",ylab="rnorm Quantiles",col="red")
  
  # return a summary of the data
  cat("min is ",min(x),"\n","1st Qu.is",quantile(x,probs=.25),"\n","median is ",median(x),"\n",'3rd Qu.is',quantile(x,probs=.75),"\n",'max is ',max(x))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~test function for my.rchisq~~~~~~~~~~~~~~~~~~~~~~~~~~~#

test.my.rchisq <- function(n,df=1){
  x<-my.rchisq(n,df=1)
  
  pass.test<-(length(x)==n & is.numeric(x))
  if(pass.test==T) print('length of generated is correct and all values are numeric')
  
  #compare true mean for chisq distribution with sample mean from my.rchisq
  u_true<-df
  u_my <- mean(x)
  cat("Theoretical mean is",u_true,"vs Sampled mean from my.rchisq is",u_my,"\n")
  
  #compare true variance for chisq distribution with sample mean from my.rchisq
  var_true <-2*df
  var_my <- var(x)
  cat("Theoretical variance is",var_true,"vs Sampled variance from my.rchisq is",var_my,"\n")
  
  #compare true median for chisq distribution with sample median from my.rchisq
  median_true <- df*(1-2/(9*df))^3
  median_my <- median(x)
  cat("Theoretical median is",median_true,"vs Sampled median from my.rchisq is",median_my,"\n")
  
  # set the output device so it shows two plots on one page
  #back up first and restore at the end
  old.par<-par("mfrow")
  par(mfrow=c(1,2))
  on.exit(par(mfrow=old.par))
  
  #plot histogram and a density line from my.rchisq
  hist(x, probability=TRUE,xlab="values from my.rchisq",main="Histogram of my.rchisq")
  lines(density(x),col="blue")
  
  #plot histogram and density line from rchisq
  y<-rchisq(n,df=1)
  hist(y, probability=TRUE,xlab="values from rchisq",main="Histogram of rchisq")
  lines(density(y),col="blue")
  
  
}


#~~~~~~~~~~~~~~~~~~~~~test function for my.rf~~~~~~~~~~~~~~~~~~~~#


test.my.rf <- function(n,df1=1,df2=1){
  x<-my.rf(n,df1=1,df2=1)
  u_my <- mean(x)
  var_my <- var(x)
  
  pass.test<-(length(x)==n & is.numeric(x))
  if(pass.test==T) print('length of generated is correct and all values are numeric')
  
  #compare true mean for F distribution with sample mean from my.rf
  if(df2>2){
    u_true<-(df2/(df2-1))
    cat("Theoretical mean is",u_true,"vs Sampled mean from my.rf is",u_my,"\n")
  }
  
  #compare true variance for F distribution with sample variance from my.rf
  if(df2>4){
    var_true <-(2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))
    cat("Theoretical variance is",var_true,"vs Sampled variance from my.rf is",var_my,"\n")
  }
  
  # set the output device so it shows two plots on one page
  #back up first and restore at the end
  old.par<-par("mfrow")
  par(mfrow=c(1,2))
  on.exit(par(mfrow=old.par))
  
  #plot histogram and a density line from my.rchisq
  hist(x, probability=TRUE,xlab="values from my.rchisq",main="Histogram of my.rchisq")
  lines(density(x),col="blue")
  
  #plot histogram and density line from rchisq
  y<-rf(n,df1=1,df2=1)
  hist(y, probability=TRUE,xlab="values from rchisq",main="Histogram of rchisq")
  lines(density(y),col="blue")
  
}
