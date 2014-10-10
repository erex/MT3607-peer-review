# I confirm that the attached is my own work
# Assignment 1 student ID 100020490 Yenlik Nurasheva

my.rnorm <- function(n, mean=0, sd=1){
# starting a function to generate pseudo-random values from uniform distribution
# using central limit theorem
#
# Args:
#   n:  n number of values to return with no default 
#   mean: mean for deviates with default value equals 0 
#   sd: standard deviation for deviates with default value equals 1
#
# Returns:
#   n numbers of random normal deviates
#Error handling
  if (n<1) stop("invalid arguments")
  # to eliminate non-integer numbers 
  if (sd<0) stop("invalid arguments")
  # to avoid negative standard deviations
  if (mean<0) stop("invalid arguments")
  # to avoid negative mean
  if (length(n)>1) stop("invalid arguments") 
  if (length(sd)>1) stop("invalid arguments")
  if (length(mean)>1) stop("invalid arguments")
  # this conditions are used to accept only scalars for arguments n, sd and mean
  if (is.nan(n)==TRUE) {warning("value n is not a number")}
  # if numeric number n is non a number, R will generate a warning message
  if (is.infinite(n)==TRUE) stop("invalid arguments")
  # to avoid infinite values as input parameters 

  X <-numeric(n)
  # create an object X of type numeric with length n
  for (i in 1:n) {
  # starting the loop from 1 to given n
    uniform<-runif(16)
    # generating 16 random deviates from uniform distribution on the interval between 0 and 1
    X[i] <-((sum(uniform)-8)*sqrt(12/16))*sd+mean
    # using central limit theorem to generate normal deviates from uniform distribution
  }
  # closing the loop
  return(X)
  # getting random normal deviates
}
# end of the function


my.rchisq  <- function(n, df=1){
# starting a function to generate n number of pseudo-random chi-squared distributed devites 
# from random standard normal deviates
# Args:
#   n:  n number of values to return with no default 
#   df: degree of freedom of standard normal deviates
#
# Returns:
#   n numbers of random chi-squared distributed deviates
#Error handling
  if (n<1) stop("invalid arguments") 
  # to eliminate non-integer numbers 
  if (n<0) stop("invalid arguments")
  # to use only positive n value 
  if (df<1) stop("invalid arguments")
  # to avoid non-integer value of degree of freedom  
  if (df<0) stop("invalid arguments")
  # eliminate negative value of degree of freedom
  if (!is.numeric(df)) stop("invalid arguments")
  # to see whether degree of freedom is numeric 
  if (is.na(df)) {warning("invalid arguments")}  
  # to spot missing values of degree of freedom 
   if (is.nan(df)) {warning("invalid arguments")}  
  # to spot values of degree of freedom which are not numbers

  chisq <- numeric(n)
  # create an object chisq of type numeric with length n
  for (i in 1:n){
     chisq[i] <- sum((my.rnorm(df))^2)
     # summing each squared standard normal deviate to get random chi-squared distributed deviates
  }
  return(chisq)
  # getting random chi-squared distributed deviates
}
# end of the function



my.rf <- function(n, df1=1, df2=1){
# starting a function to generate pseudo-random F-distributed deviates with degree of freedoms set as default 1
# Args:
#   n:  n number of values to return with no default 
#   df1: degree of freedom of first set of standard normal deviates
#   df2: degree of freedom of second set of standard normal deviates
#
# Returns:
#   n numbers of random F-distributed deviates
#Error handling   
  if (n<1) stop("invalid arguments")
  # to eliminate non-integer numbers 
  if (n<0) stop("invalid arguments")
  # to use only positive n value 
  if (df1<1) stop("invalid arguments")
  # to avoid non-integer value of degree of freedom 1  
  if (df1<0) stop("invalid arguments")
  # eliminate negative value of degree of freedom 1
  if (df2<1) stop("invalid arguments")
  # to avoid non-integer value of degree of freedom 2 
  if (df2<0) stop("invalid arguments")
  # eliminate negative value of degree of freedom 2
  if (!is.numeric(df1)) stop("invalid arguments")
  # to use only those degree of freedom 1 which are numeric 
  if (!is.numeric(df2)) stop("invalid arguments")
  # to use only those degree of freedom 2 which are numeric 
  if (is.na(df1)) {warning("invalid arguments")}  
  # to spot missing values of degree of freedom 1
  if (is.na(df2)) {warning("invalid arguments")}  
  # to spot missing values of degree of freedom 2
  if (is.nan(df1)) {warning("invalid arguments")}  
  # to spot values of degree of freedom 1 which are not numbers
  if (is.nan(df2)) {warning("invalid arguments")}  
  # to spot values of degree of freedom 2 which are not numbers
  
  f <- numeric(n)
  # create an object f of type numeric with length n
  for (i in 1:n) {
  # starting a loop from 1 to given n
    f[i]<-(my.rchisq(df1)/df1)/(my.rchisq(df2)/df2)
    # generating random F-distributed deviates from random chi-squared distributed deviates
  }
  return(f)
  # getting random F-distributed deviates 
}
# end of function
    

  
