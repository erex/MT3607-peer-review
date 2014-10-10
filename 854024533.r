#1.Return a vector of pseudo-random values from a normal distribution
my.rnorm<-function(n,mean=0,sd=1){
  X<-c()        #Generate a vector named X
  if(n<0){
    print('please enter n>=0 and an interger') #n must be integer and >= 0
  }
  else if(n!=round(n)){
    print('please enter n>=0 and an interger')
  }
  else{
  for(i in 1:n){
    Ui<-runif(16)  #Generate 16 random values from a uniform distribution
    sumUi<-sum(Ui)
    sqrt<-sqrt(12/16)
    Xi<-c((sumUi-8)*sqrt)
    X<-c(X,Xi)
    }
  return(X)
  }
}


#2.Return a vector of pseudo-random Chi-Square distributed deviates
my.rchisq<-function(n,df=1){
  Z<-c()
  if(n<0){
    print('please enter n>=0 and an interger') #n must be integer and >= 0
  }
  else if(n!=round(n)){
    print('please enter n>=0 and an interger')
  }
  else if(df!=round(df)){
    print('please enter an integer') #df must be integer 
  }
  else{
    for(i in 1:n){
  my.rnorm(df)   #Generate pseudo-random values from a normal distribution
  X<-my.rnorm(df)
  z<-X^2
  Zi<-sum(z)   #Zi is a Chi-Square distributed deviate
  Z<-c(Z,Zi)
  }
  return(Z)
}
}


#3.Return a vector of pseudo-random F-distributed deviates
my.rf<-function(n,df1=1,df2=1){
  F<-c()
  if(n<0){
    print('please enter n>=0 and an interger')
  }
  else if(n!=round(n)){
    print('please enter n>=0 and an interger') #n must be integer and >= 0
  }
  else if(df1!=round(df1)){
    print('please enter an integer')#df1 must be integer 
  }
  else if(df2!=round(df2)){
    print('please enter an integer')#df2 must be integer 
  }
  else{
  for(i in 1:n){
  U<-my.rchisq(1,df1) #Generate a Chi-Square distributed deviate U
  V<-my.rchisq(1,df2) #Generate a Chi-Square distributed deviate V
  fi<-(U/df1)/(V/df2) #fi is a F-distributed deviate
  F<-c(F,fi)
  }
  return(F)
}
}



#Test functions
#1.Test for my.rnorm
my.test1<-function(n){
  A<-my.rnorm(n)
  pass.test1<-(length(A)==n & is.numeric(A)) #Test whether the length is equal to n and the data type is numeric
  return(pass.test)
}

#2.Test for my.rchisq
my.test2<-function(n,df=1){
  B<-my.rchisq(n,df=1)
pass.test2<-(length(B)==n & is.numeric(B)) #Test whether the length is equal to n and the data type is numeric
return(pass.test2)
}

#3.Test for my.rf
my.test3<-function(n,df1=1,df2=1){
  C<-my.rf(n,df1=1,df2=1)
  pass.test3<-(length(C)==n & is.numeric(C)) #Test whether the length is equal to n and the data type is numeric
  return(pass.test3)
}