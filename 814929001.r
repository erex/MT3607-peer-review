#I confirm that the attached is my own work, except where clearly indicated in the text.

error.checks<-function(x){ stopifnot(length(x)==1, x>=0, x<1000000, x%%1==0)}
    #Purpose: Function which is to stop if: the input is not a scalar
    #                                     : the input is more than 0
    #                                     : the input is less than 1 million
    #                                     : the input is an integer value
    #Hence it stops the external function from running if there is invalid input.
    #Input: Dummy variable
    #Output: Either the error or all is TRUE.

my.rnorm<-function(n,mean=0,sd=1){
    #Purpose:Function producing normally distributed deviates from random deviates of the uniform distribution
    #Inputs: n- number of values to return
    #        mean- mean of values to return- default 0
    #        sd- standard deviation of values to return- default 1
    #Outputs: Norm.Deviates- the n normally distibuted deviates
  error.checks(n)
  if(sd<0) stop("sd must be positive") #stops function if sd is not positive
        Norm.Deviates<-numeric(n)
        for(i in 1:n){
        Uniform.Deviates<-runif(16)
        #Produces 16 random deviates from the uniform distribution
          Norm.Deviates[i]<-(((sum(Uniform.Deviates)-8)*sqrt(12/16))*sd+mean) 
        }
        #Central Limit Theorem method(Rubinstein 1981:89-90) for finding normally-distributed deviates, transformed for non-default mean and sd values.
  return(Norm.Deviates)  
}

my.rchisq<-function(n,df=1){
  #Purpose:Function producing a vector of pseudo-random chi square distributed deviates from the Normal Deviates calculated in my.rnorm function
  #Input:n- number of values to return
  #      df- degrees of freedom of the distribution-default 1
  #Output:n deviates from the Chi Squared Distibution with df degrees of freedom.
  error.checks(n)
  error.checks(df)
        Chi.Deviates<-numeric(n)
        for(i in 1:n){
          Chi.Deviates[i]<-sum(my.rnorm(df)^2)
        }
  #For loop squares the standard normal deviates and finds the summation.
  return(Chi.Deviates)
}

my.rf<-function(n,df1=1,df2=1){
  #Purpose:Function producing a vector of pseudo-random F-distributed deviates from the Chi Squared distributed deviates calculated in mr.rnorm function
  #Input:n- number of values to return
  #      df1- degrees of freedom of the numerator-default 1
  #      df2- degrees of freedom of the denominator-default 1
  #Output:n deviates from the f Distibution with df1 and df2 degrees of freedom.
    error.checks(n)
    error.checks(df1)
    error.checks(df2)
        f.Deviates<-numeric(n)
        for(i in 1:n){
          f.Deviates[i]<-(my.rchisq(1,df1)*df2)/(my.rchisq(1,df2)*df1)
        } 
  #For loop calculates the F-distribution deviates from two independent Chi-Squared distribution deviates.
  return(f.Deviates)
}

#Checks for functions

#Check for my.rnorm function
qq.plot<-function(n=1000,mean=0,sd=1){
  #Purpose:Function to show my.rnorm produces deviates from a normal distribution.
  #Input:n- number of values to return
  #      mean- mean of values to return- default 0
  #      sd- standard deviation of values to return- default 1     
  #Output:QQ-plot which should show a linear line if the results from my.rnorm are actually normally distributed.
error.checks(n)
if(sd<0) stop("sd must be positive")
qqnorm(my.rnorm(n,mean,sd))
}

#Checks for appropriate mean and variance values in functions

percentage.difference<-function(obs.value,exp.value){
  #Purpose: General function which calculates the precentage difference between input. To be used within following function checks.
  #Input: obs.value- Dummy variable for observed mean/variance.
  #       exp.value- Dummy variable for expected mean/variance.
  #Output: cat statement depending on the size of precentage difference.
  percentage<-abs(exp.value/obs.value)
  abspercentage<-abs(1-percentage)
  if((abspercentage)<=0.05) { 
    cat("the observed value,",obs.value,"is within 5% of the expected value,",exp.value,"so there is no reason to suggest my function does not work\n")
  }else{
    if((abspercentage)<=0.15) { 
      cat("the observed value,",obs.value,"is within 15% of the expected value,",exp.value,"so there is very little reason to suggest my function does not work\n")
    }else{
      if((abspercentage)<=0.25) { 
        cat("the observed value,",obs.value,"is within 25% of the expected value,",exp.value,"so there is little reason to suggest my function does not work\n")
      }else{
        cat("the observed value,",obs.value,"is not within 25% of the expected value,",exp.value,"so there may be reason to suggest my function does not work. If the observed value looks close to the expected value, try again.\n")                                 
      }}}}

mean.norm<-function(mean){
  #Purpose: Function which uses the percentage.difference function to see how close the mean from 1000 deviates, from my.rnorm, is to the expected mean of a normal distribution ie. inputed mean.
  #Input: mean-mean of function
  #Output: cat statement specifying difference between means and the resulting conclusion.
  obs.mean.norm<-mean(my.rnorm(1000,mean))
  exp.mean.norm<-mean
  percentage.difference(obs.mean.norm,exp.mean.norm)
}

var.norm<-function(sd){
  #Purpose: Function which uses the percentage.difference function to see how close the variance from 1000 deviates, from my.rnorm, is to the expected variance of a normal distribution i.e sd^2.
  #Input: df- degrees of freedom
  #Output: cat statement specifying difference between variances and the resulting conclusion.
  if(sd<0) stop("sd must be positive")
  obs.var.norm<-var(my.rnorm(1000,mean=0,sd))
  exp.var.norm<-sd^2
  percentage.difference(obs.var.norm,exp.var.norm)
}

mean.chi<-function(df){
  #Purpose: Function which uses the percentage.difference function to see how close the mean from 1000 deviates, from my.rchisq, is to the expected mean of a chi squared distribution i.e df.
  #Input: df- degrees of freedom
  #Output: cat statement specifying difference between means and the resulting conclusion.
  error.checks(df)
  obs.mean.chi<-mean(my.rchisq(1000,df))
  exp.mean.chi<-df
  percentage.difference(obs.mean.chi,exp.mean.chi)
}

var.chi<-function(df){
  #Purpose: Function which uses the percentage.difference function to see how close the variance from 1000 deviates, from my.rchisq, is to the expected variance of a chi squared distribution i.e 2df.
  #Input: df- degrees of freedom
  #Output: cat statement specifying difference between variances and the resulting conclusion.
  error.checks(df)
  obs.var.chi<-var(my.rchisq(1000,df))
  exp.var.chi<-2*df
  percentage.difference(obs.var.chi,exp.var.chi)
}

mean.f<-function(df1,df2){
  #Purpose: Function which uses the percentage.difference function to see how close the mean from 1000 deviates, from my.rf, is to the expected mean of a f distribution i.e df2/(df2-2).
  #Input: df1- degrees of freedom
  #       df2- degrees of freedom which must be more than 2
  #Output: cat statement specifying difference between means and the resulting conclusion.
  error.checks(df1)
  error.checks(df2)
  stopifnot(df2>2)
  obs.mean.f<-mean(my.rf(1000,df1,df2))
  exp.mean.f<-df2/(df2-2)
  percentage.difference(obs.mean.f,exp.mean.f)
}

var.f<-function(df1,df2){
  #Purpose: Function which uses the percentage.difference function to see how close the variance from 10000 deviates, from my.rf, is to the expected variance of a f distribution i.e 2df2^2(df1+df2-2)/(df1(df2-2)^2(df2-4).
  #Input: df1- degrees of freedom
  #       df2- degrees of freedom which must be more than 4
  #Output: cat statement specifying difference between variances and the resulting conclusion.
  error.checks(df1)
  error.checks(df2)
  stopifnot(df2>4)
  obs.var.f<-var(my.rf(10000,df1,df2))
  exp.var.f<-(2*(df2^2)*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4))
  percentage.difference(obs.var.f,exp.var.f)
}