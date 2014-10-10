#I confirm that the attached is my own work, except where clearly indicated in the text.



my.rnorm <- function(n=0,mean.1=0,sd=1) {
  x <- rep(0,n)
##This line creates a vector of length n for the n random normal deviates to be put into once created.
  for(i in 0:n){
##This line ensures that the creation of the random normal deviate is repeated n times.
     a <- runif(16,0,1)
##This line creates a vector of 16 random uniform distributions between 0 and 1
     sum.x <- ((sum(a)-8)*sqrt(12/16))
##This line converts the 16 random uniform values between 0 and 1 into a single value with a mean of 0 and stan
      x[i] <- (sum.x*sd)+mean.1}
##This line transforms each random normal deviate (with mean=0 and standard deviation of 1) into a random normal deviate with the user defined mean and user defined standard deviation.
 return (c(x))
##Finally this outputs the list of random normal deviates.
}



my.rchisq<-function(n,df=1){
  if (df<1) stop("Error: invalid argument")
##This line ensures that the function won't run when the degrees of freedom input value is 0 or negative.
##If df isn't an integer, R rounds the number down and uses that as the df value.
 {x<-rep(0,n)
 for (i in 0:n){
##This line ensures that n random Chi squared deviates are produced.
  a.sq<-rep(0,df)
  for (j in 1:df)
##This line ensures that each random deviate produced has df degrees of freedom.
   {a.sq[j]<-((my.rnorm(1,0,1))^2)}
##This line creates squared values of individual random standard normal deviates.
  x[i]<-sum(a.sq)}
##This line sums the individual squared random normal deviates to create a single random Chi-squared deviate. 
c(x)}}




my.rf <- function(n,df1=1,df2=1){
  if(df1<1) stop("Error: Invalid argument")
  if(df2<1) stop("Error: Invalid argument")
##These 2 lines stop the function when the user inputs invalid values for the 2 degrees of freedom  
    F.1<-rep(0,n)
    for(i in 0:n){
##This ensures that n F distributed deviates are produced
      U<- sum(my.rnorm(df1)^2)
      V<- sum(my.rnorm(df2)^2)
##These 2 lines create objects U and V which are Chi-squared distributed random deviates.
      F.1[i] <- ((U/df1)/(V/df2))}
##This line transforms the Chi-squared distributed deviates U and V into an F distributed deviate, with df1 and df2 degrees of freedom.
  return (c(F.1))
}




normtest.rnorm <- function(n=100){
  p.val<- shapiro.test(my.rnorm(n))$p.value  
return(c(p.val))
}

#This function calculates the p value for the sample data produced by my.rnorm. It calculates the probability that the sample data came from a normal distribution. The only output is this p-value and so a user of the function will need to know how to interpret this value. After running the code myself i found that my.rnorm would fail this test (get a p value of less than 0.05) around 1 in 20 times, depending on the sample size, which is expected.


meantest.rnorm<-function(n=100,mean.1=100){
    a<-(mean(my.rnorm(n,mean.1))/mean.1)
    return(a)
}

#This function tests the mean of a user defined number of random normally distributed deviates created by the my.rnorm function. It calculates the mean of n (default 10000) deviates and then divides this value by the (user inputted) mean to give a value easier to interpret. If the mean of the random deviates is exactly the same as the mean inputted to my.rnorm, a value of 1 will be the output. I ran this test many times with varying means and values of n and the output was generally always pretty close to 1, most often between 0.95 and 1.05.
#An issue with the method i have used is that if the user inputted mean is very small then the values become much more varied around 1. 


sdtest.rnorm<-function(n=1000,sd.1=5){
  a<-(sd(rnorm(n,sd=sd.1))/sd.1)
return(a)}

#This tests the standard deviation of a user defined number of random normal deviates created by my.rnorm. The output is the standard deviation of the deviates divided by the standard deviation itself to get a ratio for easier analysis. 
#Out of 30 runs of this test, all outputs were between 0.95 and 1.05, giving good evidence that my.rnorm is correctly producing deviates with the user defined sd value.


meantest.rchisq<-function(n=1000){
a<-mean(my.rchisq(n))  
return(a)
}

#This tests whether the mean of a user defined number of chi-squared distributed values produced by the my.rchisq function is the mean that it is supposed to produce. My testing showed the means were centred around 1 and almost always between 0.98 and 1.02, as is expected.
