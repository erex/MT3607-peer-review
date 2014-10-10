#MT3607 Assignment 1
# I confirm that the attached is my own work, except where clearly indicated in the text.


 my.rnorm<-function(n,mean=0,sd=1){
#return a vector of pseudo-random values from a normal distribution
#use Central limit theorem method
#the function is designed to accept only scalars argument
#Inputs:
#n: number of values to return - no default
#mean: mean of values to return - default 0
#sd: standard deviation of values to return - default 1
#Outputs:
#a vector of pseudo-random values from a normal distribution
#if there are some error, checks inputs
 X<-numeric(n)
#create a vector to put normal deviates in
 for(i in 1:16){
#generate 16 uniform(0,1)
 U<-runif(16)
 N<-(sum(U)-8)*sqrt(12/16)
#sum 16 uniform deviates and subtract 8, and then multiply by square root of 12/16,to get normal deviate 
 X[i]<-(N*sd)+mean
#transform normal deviate into values with mean and sd by multiplying the values by sd and then adding mean
 }
 return(X)
#output results
 }



 my.rchisq<-function(n,df=1){
#returning a vector of pseudo-random chi square distributed deviates
#the function will not use the non-centrality parameter and df only needs to take on integer values
#Inputs:
#n: number of values to return - no default
#df: degrees of freedom of the distribution - default 1
#Outputs:
#a vector of pseudo-random chi square distributed deviates
#if there are some error, checks inputs
 R<-numeric(n)
#create a vector to put chi squared distributed deviates in
 for(i in 1:n){
#generate n standard normal distributed deviates
 chi<-sum(my.rnorm(n)^2)
#sum the square of normal distributed deviates to get chi squared deviates
 R[i]<-chi
 }
 return(R)
#output results
 }



 my.rf<-function(n,df1=1,df2=1){
#returning a vector of pseudo-random F-distributed deviates
#the function will not use the non-centrality parameter
#Inputs:
#n: number of values to return - no default
#df1: degrees of freedom of the numerator - default 1
#df2: degrees of freedom of the denominator - default 1
#Outputs:
#a vector of pseudo-random F-distributed deviates
#if there are some error, checks inputs
 F<-numeric(n)
#create a vector to put f-distributed deviates in
 for (i in 1:n){
 U<-my.rchisq(df1)
 V<-my.rchisq(df2)
#generate 2 independent chi squared distributed deviates. Here df1 and df2 belongs to n and may be different with each other.
 F[i]<-(U/df1)/(V/df2)
#generate f distributed deviates
 }
 return(F)
#output results
 }





#TEST CODE:
#> my.rnorm<-function(n,mean=0,sd=1){
#+ if(n<0){
#+ stop("n must be positive")
#+ }
#+ if(length(n)>1){
#+ stop("The 'n' argument should be scalar")
#+ }
#+ if(length(mean)>1){
#+ stop("The 'mean' argument should be scalar")
#+ }
#+ if(length(sd)>1){
#+ stop("The 'sd' argument should be scalar")
#+ }
#+ if(!is.finite(n)){
#+ stop("n should be finite")
#+ }
#+ if(!is.finite(mean)){
#+ stop("mean should be finite")
#+ }
#+ if(!is.finite(sd)){
#+ stop("sd should be finite")
#+ }
#+ if(n!=round(n)){
#+ stop("n should be an integer")
#+ }
#+ if(sd<=0){
#+ stop("sd should be >0")
#+ }
#+ X<-numeric(n)
#+ for(i in 1:16){
#+ U<-runif(16)
#+ N<-(sum(U)-8)*sqrt(12/16)
#+ X[i]<-(N*sd)+mean
#+ }
#+ return(X)
#+ }




#> my.rchisq<-function(n,df=1){
#+ if(n<0){
#+ stop("n must be positive")
#+ }
#+ if(length(n)>1){
#+ stop("The 'n' argument should be scalar")
#+ }
#+ if(df<0){
#+ stop("df must be positive")
#+ }
#+ if(!is.finite(n)){
#+ stop("n should be finite")
#+ }
#+ if(!is.finite(df)){
#+ stop("df should be finite")
#+ }
#+ if(n!=round(n)){
#+ stop("n should be an integer")
#+ }
#+ if(df!=round(df)){
#+ stop("df should be an integer")
#+ }
#+ R<-numeric(n)
#+ for(i in 1:n){
#+ chi<-sum(my.rnorm(n)^2)
#+ R[i]<-chi
#+ }
#+ return(R)
#+ }



#> my.rf<-function(n,df1=1,df2=1){
#+ if(n<0){
#+ stop("n must be positive")
#+ }
#+ if(length(n)>1){
#+ stop("The 'n' argument should be scalar")
#+ }
#+ if(df1<0){
#+ stop("df1 must be positive")
#+ }
#+ if(df2<0){
#+ stop("df2 must be positive")
#+ }
##df1 and df2 must be positive(Infinite is allowed)
#+ if(!is.finite(n)){
#+ stop("n should be finite")
#+ }
#+ if(n!=round(n)){
#+ stop("n should be an integer")
#+ }
#+ F<-numeric(n)
#+ for (i in 1:n){
#+ U<-my.rchisq(df1)
#+ V<-my.rchisq(df2)
#+ F[i]<-(U/df1)/(V/df2)
#+ }
#+ return(F)
#+ }


