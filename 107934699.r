#I confirm that the attached is my own work,except where clearly indicated in the text
my.rnorm<-function(n,mean,sd){
  X<-vector()#set X as a vector
  if(round(n)==n && sd>=0 && n>0){#n is positive integer, sd is positive number
    for (m in 1:n){
      u<-matrix(runif(16)) 
      normal<-((colSums(u)-8)*(12/16)^0.5)
      x<-normal*sd+mean
      X[m]<-x} #input values in vector X
    print(X)#cat command doesn't work on first test,don't know why
  }else{
    cat("invalid arguments")
  }
}


my.rchisq<-function(n,df){
  chisq<-vector()#set chisq as a vector
  if(round(n)==n && round(df)==df && n>0 && df>0){#n,df are all positive integers
    for (m in 1:n){
      chi<-0                  #set chi 0 for each loop
      for (l in 1:df){
        x<-matrix(runif(16)) 
        X<-((colSums(x)-8)*(12/16)^0.5)
        chi<-X^2+chi
      }
      chisq[m]<-chi #input values in vector chisq
    }
  print(chisq)
  }else{
    cat("invalid arguments")
  }   
}



my.rf<-function(n,df1,df2){
  if(round(n)==n && round(df1)==df1 && round(df2)==df2 && n>0 && df1>0 && df2>0){#n,df1,df2 are all positive integer
    ftest<-vector()#set ftest as a vector 
    for (m in 1:n){          
      u1<-u2<-0             
      for (a in 1:df1){
        x1<-matrix(runif(16)) 
        x1<-((colSums(x1)-8)*(12/16)^0.5)
        u1<-x1^2+u1
      }
      for (b in 1:df2){
        x2<-matrix(runif(16)) 
        x2<-((colSums(x2)-8)*(12/16)^0.5)
        u2<-x2^2+u2
      }
      ftest[m]<-(u1/df1)/(u2/df2)
     } 
   print(ftest)
  }else{
    cat("invalid arguments")
  }
}



#source('asmt1.r')

test1<-function(n,mean,sd){#test normality 
  result1<-my.rnorm(n,mean,sd)
  shapiro.test(result1)
  cat("normality is",shapiro.test(result1)$p.value>0.05,"\n")
}
#normality is true means the p value in shapiro-wilk test is greater than 0.05,
#and data are normally distributed. normality is false means data don't follow 
#normal distribution

test2<-function(n,df){#test whether chi-square mean is close to df
  result2<-my.rchisq(n,df)
  round(mean(result2))==df 
}
#check whether the mean is close to df,if is true, the data should be
#chi-squared distributed.If it is false,the data is not chi-squared distributed.
#Try larger n.
                         

