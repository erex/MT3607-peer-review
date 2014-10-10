#I confirm that the attached is my own work, except where clealy indicated in the text.

#please enter the number you wanna create:n
#please enter the mean of the normal distribution:mean
#please enter the standard deviation of the normal distribution:sd
#the default mean is 0, and the default sd is 1
print('enter the number you wanna create:n, mean, sd')
my.rnorm<-function(n,mean=0,sd=1){
  
  #to see the numbers input are all in the right formal
  if(n<=0){
    print('invalid number, please enter a positive number')
    
  }
  else if(sd<=0){
    print('invalid sd, please enter a positive number')
  }
  else if(n!=round(n)){
    print('please enter an integer')
  }
  
  #to create numbers have normal distribution
  else {
    x1<-numeric(n)
    for(i in 1:n){
      #u: 16 random numbers have uniform distribution
      u<-runif(16)
      su<-sum(u)
      x1[i]=(su-8)/sqrt(16/12)
      i=i+1      
    }
    #secondly, transform x1 into values with mean and sd
    x<-numeric(n)
    for(i in 1:n){
      x[i]=sd*x1[i]+mean
      i=i+1
    }
    return(x)
  }
}

############################################################################


#please enter the number you wanna create:n
#please enter the degree of freedom:df
#the default number of df is 1
print('enter the number you wanna create and the degree of freedom')
my.rchisq<-function(n,df=1){
  
  #to see the numbers input are all in the right formal
  if (n<=0){
    print('invaild number,please enter a positive number')
    
  }
  else if(df<=0){
    print('please enter a positive number for df')
  }
  else if(df!=round(df)){
    print('please enter an integer for df')
  }
  else if(n!=round(n)){
    print('please enter an integer for n')
  }
  
  #to create numbers have chisq distribution
  else {  
    ch<-numeric(n)
    for(i in 1:n){
      rn<-my.rnorm(df)
      ch[i]=sum(rn^2)
      i=i+1
    }  
    return(ch)
  }
  
}

###############################################################################

#please enter the number you wanna create:n
#please enter the degree of freedom:df1, df2
print('enter the number you wanna create and two degrees of freedom')
my.rf<-function(n,df1=1,df2=1){
  
  #to see the numbers input are all in the right formal
  if(n<=0){
    print('please enter a positive number for n')
  }
  else if(n!=round(n)){
    print('please enter an integer for n')
  }
  else if(df1<=0){
    print('please enter a positive number for df1')
  }
  else if(df2<=0){
    print('please enter a positive number for df2')
  }
  else if(df1!=round(df1)){
    print('please enter an integer for df1')
  }
  else if(df2!=round(df2)){
    print('please enter an integer for df2')
  }
  
  #to create numbers have F distribution
  else{
    fd<-numeric(n)
    for(i in 1:n){
      ff<-my.rnorm(df1)
      fff<-my.rnorm(df2)
      fd[i]<-((sum(ff^2))/df1)/((sum(fff^2))/df2)
      i=i+1
    }
    return(fd)
  }
}

###############################################################################


#first create a x vector by using my.rnorm
#add the real mean and sd
my.normtest<-function(x,mean,sd){
  
  #to see wether the number the function create is right 
  n<-10
  te<-my.rnorm(n,mean,sd)
  if(length(te)==n){
    print('right amount of the size')
  }
  
  par(mfrow=c(1,3))
  #to see the difference between the real normal distribution and the sample
  hist(x,freq=FALSE)
  lines(density(x),col="blue")  
  mi<-min(x)
  ma<-max(x)
  w<-seq(mi-1,ma+1,by=0.01)
  lines(w,dnorm(w,mean,sd),col="red")
  
  #draw the qqplot
  qqnorm(x);
  qqline(x);
  
  #draw the empirical distribution function
  plot(ecdf(x),verticals=TRUE,do.p=FALSE)
  lines(w,pnorm(w,mean(x),sd(x)))
   
  #to see the result from Shapiro-Wilk test
  shapiro.test(x) 
  #if the p-value in shapiro test is greater than 0.05, then we consider the
  #sample comes from normal distribution
  
}