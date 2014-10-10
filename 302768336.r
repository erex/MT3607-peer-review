#University of St Andrews
#MT3607 - Computing in Statistics
#Assignment 1
#______________________________________________________________________________#


#CALL FUNCTION my.rnorm
my.rnorm<-function(n,mean=0,sd=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#PURPOSE: This R function will generate a vector of pseudo random values that
#         falls within a normal distribution based on the following algorithm:
#
#         Let i=1 to 16
#         Generate Ui ~ uniform(0,1)
#         Let x={for (i in 1:16){(sum(Ui)-8)*sqrt(12/16)}}
#
#         
#INPUTS:   n     :=number of observations - no default
#          mean  :=mean of values to return - default 0
#          sd    :=standard deviation of values to return - default 1
#
#OUTPUTS:  f     :=vector of pseudo random valus from normal distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________#
    
    #variable declaration
    u<-numeric(16)
    usum<-0
    x<-0
    xnew<-numeric(n)
    
    
    #error traps
    if(n%%1 !=0) {stop ("Invalid argument")
                  
    }else
      if (n<1) {stop("Invalid argument")
                
      }else
        if (sd<0) {stop("Invalid argument")
        }else
          
          #Loop
          for (i in 1:n){
            u<-runif(16)    #generate ui~uniform(0,1)
            usum<-sum(u)
            
            x<-(usum-8)*sqrt(12/16)
            
            #Transform values with mean 0 and sd 1 into values with mean and sd 
            xnew[i]<-x*sd+mean   
            
            
          }
    
    #Print results
    return(xnew)
  }
#__________________________________~END~_______________________________________#
#
#I confirm that the attached is my own work, except where clearly indicated in the
#text.
#______________________________________________________________________________#
#
#
#CALL FUNCTION my.rchisq
my.rchisq<-function(n,df=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#PURPOSE: This R function will greate a vector of pseudo-random valus from chisq 
#         distributed deviates using the theorem:
#
#         Let Z1,Z2,Z3,...,Zn be n independent standard normal random variables.
#         Then:
#              for(i in 1:n) {sum(Zi^2)} ~ chisq(n)
#
#INPUTS:      n :=number of observations - no default
#            df :=degrees of freedom - default 1
#
#OUTPUTS:  chsq :=vector of pseudo random valus that falls within chisq 
#                 distribution                          
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________#

    
  #variable declaration
  
  z<-numeric(df)
  zsquare<-numeric(df)
  chsq<-numeric(n)
  
  #error traps

   if (n%%1 != 0) {stop("Invalid argument")
    }else
     if (df%%1 != 0){ stop ("Invalid argument")
     }else
      if (n<1) {stop("Invalid argument")
      }else
        if (df<0){ stop("Invalid argument")
        }else
          
          
          #Loop 
          for (i in 1:n){
            z<-my.rnorm(df)   #z1,z2,...zdf ~N(0,1)
            zsquare<-z^2
            chsq[i]<-sum(zsquare)
          }
  
  #Print Results
  return(chsq)
}
#__________________________________~END~_______________________________________#
#                                                                             
#I confirm that the attached is my own work, except where clearly indicated in 
#the text.
#
#______________________________________________________________________________#
#
#
#CALL FUNCTION my.rf
my.rf<-function(n,df1=1,df2=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PURPOSE: R function that returns a vector of pseudo-random F-distributed 
#         deviates using the distribution theory theorem:
#
#         Let U={for(i in 1:m){sum(chisq(i))}} 
#         and V={for(i in 1:n){sum(chisq(i))}}
#         where Xi~N(0,1), i=1,2,3,...,m and Yi~N(0,1), i=1,2,...,n
#         and all Xi's and Yi's are independent. Then:
#                   F={(U/m)/(V/n)}
#
#INPUTS:  n   :=number of observations - no default
#         df1 :=degrees of freedom of the numerator - default 1
#         df2 :=degrees of freedomm of the denominator - default 1
#
#OUTPUTS: F   :=vector of pseudo random valus from F distribution
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________#


  #variable declaration
  u<-numeric(df1)
  u<-numeric(df2)
  fdf<-numeric(n)
  udf<-0
  vdf<-0
  
  #error traps
  if (n%%1 != 0) {stop ("Invalid argument")
  }else
    if (n<1) {stop ("Invalid argument")
    }else
      if (df1%%1 !=0 ) {stop ("Invalid argument")
      }else
        if (df2%%1 !=0 ) {stop ("Invalid argument")
        }else
          if (df1<0) {stop ("Invalid argument")
          }else
            if (df2<0) {stop ("Invalid argument")
            }else
              
              
              #Loop
              for (i in 1:n){
                
                u<-my.rchisq(1,df1)   #u~chiq square(df1)
                v<-my.rchisq(1,df2)   #v~chiq square(df2)
                udf<-u/df1
                vdf<-v/df2
                fdf[i]<-(udf/vdf)
              }
  
  #Print results
  return(fdf)
}
#__________________________________~END~_______________________________________#
#
#I confirm that the attached is my own work, except where clearly indicated in 
#the text.
#
#______________________________________________________________________________#
#
#
#CALL FUNCTION test.myrnorm
test.myrnorm<-function(a,b,n,mean=0,sd=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PURPOSE: R function that test my rnorm function by drawing two qqplots and two 
#         Histograms. The first it takes numbers from rnorm function and the 
#         second one from my.rnorm fun.
#
#INPUTS:  a    :=the starting value of the sequence - no default
#         b    :=the end value of the sequence - no default
#         n    :=number of observations - no default
#         mean :=mean of values - default 0
#          sd  :=standard deviation of values - default 1
#
#OUTPUTS: no outputs. Only two QQplots
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________#
  
  
  if(n%%1 !=0) {stop ("Invalid argument")
                
  }else
    if (n<1) {stop("Invalid argument")
              
    }else
      if (sd<0) {stop("Invalid argument")
      }else
      
      
  par(mfrow=c(2,3))
  x<-seq(a,b,length=n)
  
  #draw a data qqplot
  y<-rnorm(n,mean,sd)
  qqplot(x,y,main = "Normal Q-Q Plot", col='red')
  hist(y,main = "Normal Histogram", col='red')
  boxplot(y,main = "Normal Boxplot", col='red')
  
  #draw a data qqplot
  z<-my.rnorm(n,mean,sd)
  qqplot(x,z,main = "My Normal Q-Q Plot",col='blue')
  hist(z,main = "My Normal Histogram",col='blue')
  boxplot(z,main = "My Normal Boxplot",col='blue')
}
#__________________________________~END~_______________________________________#
#______________________________________________________________________________#
#
#
#CALL FUNCTION test.myrchisq
test.myrchisq<-function(a,b,n,df=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#PURPOSE: R function that test my Chi square function by drawing two Histograms.  
#         The first it takes numbers from rchisq function and the second one 
#         from my.rchisq function.
#
#INPUTS:  a    :=the starting value of the sequence - no default
#         b    :=the end value of the sequence - no default
#         n    :=number of observations - no default
#         df   :=degrees of freedom - default 1
#
#OUTPUTS: no outputs. Only two Histograms
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________#
  
  if (n%%1 != 0) {stop("Invalid argument")
  }else
    if (df%%1 != 0){ stop ("Invalid argument")
    }else
      if (n<1) {stop("Invalid argument")
      }else
        if (df<0){ stop("Invalid argument")
        }else
          
          
          
  par(mfrow=c(2,1))
  x<-seq(a,b,length=n)
  
  #draw a data histogram 
  y<-rchisq(n,df)
  hist(y,main='Chi square Histogram',col='red')
  
  #draw a data histogram 
  z<-my.rchisq(n,df)
  hist(z,main='My Chi square Histogram',col='blue')
}

#__________________________________~END~_______________________________________#
#______________________________________________________________________________#
#
#
#CALL FUNCTION test.myrf
test.myrf<-function(a,b,n,df1=1,df2=1){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#PURPOSE: This R function will test the my.rf function by draw two histograms
#         the first histogram is from rf distribution and the second one is from
#         my rnorm 
#         
#         
#INPUTS:   a   :=the starting value of the sequence - no default   
#          b   :=the end value of the sequence - no default
#          n   :=number of observations - no default
#          df1 :=degrees of dreedom - default 1
#          df2 :=degrees of dreedom - default 1
#
#OUTPUTS: no outputs. Only two Histograms. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
#_________________________________~START~______________________________________# 
  
  if (n%%1 != 0) {stop ("Invalid argument")
  }else
    if (n<1) {stop ("Invalid argument")
    }else
      if (df1%%1 !=0 ) {stop ("Invalid argument")
      }else
        if (df2%%1 !=0 ) {stop ("Invalid argument")
        }else
          if (df1<0) {stop ("Invalid argument")
          }else
            if (df2<0) {stop ("Invalid argument")
            }else
              
              
  par(mfrow=c(2,1))
  x<-seq(a,b,length=n)
  
  #draw a data histogram 
  y<-rf(n,df1,df2)
  hist(y,main='F Histogram',col='red')
  
  
  #draw a data histogram 
  z<-my.rf(n,df1,df2)
  hist(z,main='My F Histogram',col='blue')
  
}
#__________________________________~END~_______________________________________#
#______________________________________________________________________________#
#I confirm that the attached is my own work, except where clearly indicated in  
#the text.