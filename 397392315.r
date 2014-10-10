#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){
#Purpose:Generates random normal deviates from uniform distrubution
#Inputs: 
#n - a numerical value
#mean - default 0
#sd - default 1
#Outputs: a vector of n pseudo-random values from a normal distribution
 
#errors to ensure correct input
  if(missing(n)) 
    return("invalid arguments") 
  if(length(mean)>1)
    return("invalid arguments")
  if (length(sd)>1)
    return("invalid arguments")
  if (length(n)>1)
    return("invalid arguments")
  if (n<=0)
    return("invalid arguments")

#set x to concatenate n outputs
  x<-c(1:n)
   
    for (i in c(1:n)){

#taking random uniform deviates and transforming to normal distributed deviates (Rubinstein 1981:89-90)
    u<-runif(n)
    x[i]<-((sum(u)-(0.5*n))*(sqrt(12/n)))
    
   }
#transforming the values for specific mean and sd
  return((x*sd)+mean)
}

my.rchisq<-function(n, df=1){
#Purpose: generates chi squared distributed deviates, starting from uniform deviates
#Inputs: 
#n - a numerical value
#df - degrees of freedom (default 1)
#Outputs: a vector of n pseudo-random chi-squared deviates

#errors to ensure correct input
 if (missing(n))
    return("invalid arguments")
 if (length(df)>1)
    return("invalid arguments")
 if (length(n)>1)
    return("invalid arguments")
 if (n<=0)
    return("invalid arguments")
 if (df<=0)
    return("invalid arguments")

#set both z & x to concatenate df & n outputs  
  z<-c(1:df)
  x<-c(1:n)
  
  for (k in 1:n){   
    for (i in c(1:df)){
      
     v<-runif(df)
     z[i]<-((sum(v)-(0.5*df))*(sqrt(12/df)))
     v2<-(z)^2
     U<-sum(v2)
#generate df amount of uniform devaites, tranform to normal deviates, square and sum
      
   x[k]<-U
#repeat this n times to get n chi-squared distribution deviates
    }
  }
  return(x)
}

my.rf<-function(n, df1=1, df2=1){
#Purpose: generates F-distributed deviates, starting from uniform deviates
#Inputs:
#n - a numerical value
#df1 - degrees of freedom (default 1)
#df2 - degrees of freedom (default 1)
#Outputs: a vector of n pseudo-random F-distributed deviates

#errors to ensure correct input
  if(missing(n))
    return("invalid arguments")
  if(length(n)>1)
    return("invalid arguments")
  if (length(df1)>1)
    return("invalid arguments")
  if (length(df2)>1)
    return("invalid arguments")
  if (n<=0)
    return("invalid arguments")
  if (df1<=0)
    return("invalid arguments")
  if (df2<=0)
    return("invalid arguments")

#set f,y1,y2 to concatenate n, df1 & df2 outputs respectively
  f<-c(1:n)
  y1<-c(1:df1)
  y2<-c(1:df2)

  for(k in 1:n)  { 

    for (i in c(1:df1)){

#tranforming df1 amount of uniform deviates to normal then to chi-squared      
      r<-runif(df1)
      y1[i]<-((sum(r)-(0.5*df1))*(sqrt(12/df1)))
      rsq1<-(y1)^2
      U<-sum(rsq1)

    for (j in c(1:df2)){

#tranforming df2 amount of uniform deviates to normal then to chi-squared        
      p<-runif(df2)
      y2[j]<-((sum(p)-(0.5*df2))*(sqrt(12/df2)))
      rsq2<-(y2)^2
      V<-sum(rsq2)
        
  f[k]<-(U/df1)/(V/df2)

      }
    }
  }
#generating F-distributed deviates

  return(f)
}

test.norm<-function(x){
#Purpose: test to check that data is from a normal distribution
#Inputs:
#need to call my.rnorm
#Outputs:
#draws 3 plots of the data
  
#setting to output 3 plots
  par(mfrow=c(1,3))

#draw a histogram that adds a density line
  hist(x, probability=T)
  lines(density(x))

#normal probability plot adding in the line y=x
  qqnorm(x)
  qqline(x)

#plots a boxplot
  boxplot(x, horizontal=T)
}

test.rf<-function(x){
#Purpose: test to check that data is from a F-distribution
#Inputs:
#need to call my.rf
#Outputs:
#draws 1 plot of the data

#resetting output plots
  par(mfrow=c(1,1))

#draw a histogram and add in line y=x
  hist(x, probability=T)
  lines(density(x))
  
}

test.rchisq<-function(x){
#Purpose: test to check that data is from a chi-squared distribution
#Inputs:
#x - numeric vector
#Outputs:
#chi-squared test with simulated p-value
  
#H0: data is from a chi-squared distribution
#H1: data is not from chi-squared distribution
#if p-value>0.10 - no evidence against H0
  chisq.test(my.rchisq(x,1),y=NULL, simulate.p.value=T)
}
