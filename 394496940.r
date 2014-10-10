my.rnorm <- function(n,mean,sd){
  # This function generates psudo random numbers where n has no default value.
  # The input parameters are x=n, mean, sd
  # The output parameters are psudo random values given the value of x
  
  
  u<- runif(0,1) 
  s<-sum(u)
  # s is the summation of the uniformly distributed deviates from x=1 to x=n
  x<-((s-n/2)*sqrt(n/12))
  # x is normally distributed with a given mean and sd
  a<-s*sd+mean
  # a is transformation of x into a variable with any given mean and sd
  
  return(x)
}
my.rchisq<-function(z,df){
  # This function generates psudo random numbers with a special gamma distribution referred to as the chi-square distribution.
  # The input parameters are z=n and df
  # The output parameters are psudo random numbers with a given value of z.
   x<-sum(z^2)
  return(x)
}
my.rf<-function(x=m,y=n,df1,df2){
  #This function generates psudo random variables with an F distribution where x=m,y=n, df1,df2.
  # df1 and df2 are the degrees of freedom for x and y respectively.
  u=sum(x)
  v=sum(y)
  f<-u/v
  return(f)

  }
# The following tests were carried out to check for syntax errors;
# my.rnorm(10,6,2)
# my.rchisq(16,5)
# my.rf(15,12,5,4)
# The following test was used to test the my.norm function.
# The same test can be used to test when x holds any numeric value n.
x<-numeric(5)
for(i in 1:5){
  x[i]<-my.rnorm(5,0,1)
  
}
print(x)
# The following test was used to test was used to test the my.rchisq function.
x<-my.rchisq(10)
for(i in 1:10){
  x[i]<-my.rchisq(10,1)
}
print(x)
# The test works for the my.chisq function therefore it should work for the my.rf function since the f function is the quotient of two chi-square functions.

  

