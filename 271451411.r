## Uniform to Normal 
my.rnorm <- function(n, mean.norm=0, sd.norm=1){
  x <- rep(0,n)
  for(i in 0:n){
    if(sd.norm<0) stop("sd.norm must be >0")
    if(n<0) stop("n must be >=0")
    
    ## 16 runif values used to calculate 1 normal random deviate
    r.unif <- runif(16,0,1)
    s.unif <- sum(r.unif)
    x[i] <- ((sd.norm*(s.unif-8)*sqrt(12/16))+mean.norm)
  }
  return(c(x))
}

my.rnorm(7,0,1)

## Eric's example test
x <- my.rnorm(n=10)
pass.test.1 <- (length(x)==10 & is.numeric(x))
pass.test.1

## Sharipo test with high p-value indicates normality
shapiro.test(my.rnorm(1000,0,1))

par(mfrow=c(2,1)) 
test.norm.1 <- my.rnorm(10000,0,1)
qqnorm(test.norm.1)
qqline(test.norm.1, col="red")
hist(my.rnorm(10000,0,1))
## From inspection we can see what looks like a normal distribution
## qqnorm and qqline are close for large n 
## Conclude random deviates follow normal distribution



## Normal to Chi 
my.rchisq <- function(n, df=1){
  y <- rep(0,n)
  for(i in 0:n){
    if(n<=0) stop("n must be >0")
    if(df<=0) stop("df must be >0")
    r.chisq <- my.rnorm(df,0,1)
    sq.chisq <- sum(r.chisq^2)
    y[i] <- sq.chisq
  }
  return(c(y))
}

my.rchisq(10,1)

## Eric's example test
y <- my.rchisq(n=10)
pass.test.2 <- (length(x)==10 & is.numeric(x))
pass.test.2

## Test that values are >0 & produces 10 values
neg.chisq <- my.rchisq(n=10)
pass.test.3<- (length(neg.chisq)==10 & neg.chisq>0)
pass.test.3

mean(my.rchisq(10000,1))
## Mean is close to df, supports chi distribution 
var((my.rchisq(10000,1)))
## Variance close to 2*df, supports chi distribution 

hist(my.rchisq(1000,200))
shapiro.test(my.rchisq(100,2000))
## approaches normal dist. for large df
## Sharipo test gives high p-value indicating normal distribution


## Chi to F 
my.rf <- function(n,df1=1,df2=1){
  z <- rep(0,n)
  for(i in 0:n){
    if(n<=0) stop("n must be >0")
    if(df1<=0) stop("df1 must be >0")
    if(df2<=0) stop("df2 must be >0")
    
    r.U <- my.rchisq(1,df1)
    r.V <- my.rchisq(1,df2)
    calc.F <- (r.U/df1)/(r.V/df2)
    z[i] <- calc.F
  }
  return(c(z))
}
my.rf(10,1,1)
hist(my.rf(1000,1,1))

## Eric's example test
z <- my.rf(n=10)
pass.test.4 <- (length(x)==10 & is.numeric(x))
pass.test.4

## Produces 10 values & all >0
neg.rf <- my.rf(n=10)
pass.test.5 <- (length(neg.rf)==10 & neg.rf>0)
pass.test.5