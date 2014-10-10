#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1) { #creates function with appropriate arguments
  if (is.numeric(n)) { #creates error trap for invalid arguments
    result<-c() #creates empty list
    i<-1
    while(i<n+1) { #while loop to run n times
      u<-runif(n)  #generates n random 'uniformly-distributed' deviates
      x<-(sum(u)-(n/2))*(sqrt(12/n)) #C.L.T. to give a 'N(0,1)-distributed' deviate
      result<-append(result,x) #adds x to list
      i<-i+1 } 
    return((result*sd)+mean) #can change result to any normal distribution of choice
  } else {(return("INVALID ARGUMENTS"))}}

my.rchisq<-function(n,df=1) { #creates function
  if (is.numeric(n)) { #error trap
  } else  {(return("INVALID ARGUMENTS"))} 
  result<-c() 
  result1<-c() #empty lists
  for (j in 1:n) { #for loop to run the following code n times:
    i<-1
    while(i<df+1) { #while loop to create df standard normal deviates
      u<-runif(df)
      x<-(sum(u)-(df/2))*(sqrt(12/df))
      y<-x^2
      result1<-append(result1,y) #compiles list of n N(0,1)-distributed' deviates squared
      i<-i+1 } 
    result[j]<-sum(result1[j]) } #creates n 'Chi-squared-disbributed' deviates
  return(result)} #outputs these in a vector

my.rf<-function(n,df1=1,df2=1){  
  if(is.numeric(n)) {
  } else  {(return("INVALID ARGUMENTS"))} #error trap
  result<-c()
  result1<-c()
  result2<-c()
  result3<-c()
  result4<-c() #empty lists
  for (j in 1:n) { #for loop to run n times
    i<-1
    while(i<df1+1) { #while loop to create list of df1 'N(0,1)-distributed deviates' squared
      u<-runif(df1) 
      x<-(sum(u)-(df1/2))*(sqrt(12/df1)) 
      y<-x^2
      result1<-append(result1,y)
      i<-i+1 } 
    result2[j]<-sum(result1[j]) #creates n 'Chi-squared-disbributed' deviates
  } #closes for loop
  for (k in 1:n) { #second for loop to also run n times
    l<-1
    while(l<df2+1) { #creates df2 'N(0,1)-distributed deviates' squared
      u<-runif(df2)
      x<-(sum(u)-(df2/2))*(sqrt(12/df2))
      y<-x^2
      result3<-append(result1,y) #compiles list of length df2
      l<-l+1 } 
    result4[k]<-sum(result3[k])   
    a<-c(result2,result4)} #lists n deviates of df1 deg. of freedom alongside n deviates of df2 deg. of freedom
  for (m in 1:(n))  
    result[m]<-(a[m]/df1)/(a[2*m]/df2) #takes deviates from first n and second n values and computes n 'F-distributed' deviates
  return(result) }

is.normal<-function(x) { #test to see if x=my.rnorm(n) produces normal data
  if (is.numeric(x)) { #error trap
  } else  {(return("INVALID ARGUMENTS"))} 
  a<-shapiro.test(x) #'shapiro.test' gives p values for normality
  if (a$p.value>0.05) { 
    return("TRUE") #returns TRUE if we reject the null hypothesis at 5% level
  } else {return("FALSE")}}

norm.plot<-function(n) { #test to see if my.norm(n) produces normal data
  if (is.numeric(n)) { #error trap
  } else  {(return("INVALID ARGUMENTS"))} 
  old.par<-par(mfrow=c(1,2))
  qqnorm(my.rnorm(n), main = "my.rnorm(n) Q-Q plot"); qqline(my.rnorm(n)) #creates Q-Q Plot to visualise normality of data
  qqnorm(rnorm(n), main = "rnorm(n) Q-Q plot"); qqline(my.rnorm(n))
  par(old.par)} #creates another to compare with R's default random normally-distrubted deviates
#values of n>1000 clearly highlight the normality of both

plot.chis.mean.diff<-function(n) { #creates plot of the mean difference between n values given by rchisq and my.rchisq
  if (is.numeric(n)) { #error trap
  } else  {(return("INVALID ARGUMENTS"))} 
  result<-c()
  i<-1
  while(i<n+1) { 
    a<-sum(abs(my.rchisq(i,df=1))) #sums values of my.rchisq
    b<-sum(abs(rchisq(i,df=1))) #sums values of rchisq
    x<-(abs((b-a)/n))
    result<-append(result,x) 
    i<-i+1 } 
  return(plot(result,ylim=c(0,n)))} #for large n (n>20), a straight line at y=0 implies random deviates produced are overall very similar

f.same.type<-function(n,df1,df2) {
  if (is.numeric(n)) {
  } else  {(return("INVALID ARGUMENTS"))} 
  a<-my.rf(n,df1,df2)
  b<-rf(n,df1,df2)
  if (typeof(a)==typeof(b)) {
    return(TRUE)  
  } else (return(FALSE)) }
#basic function to test if data produced from my.rf(n) and rf(n) is of the same type