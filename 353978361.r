#MT3607: Computing in Statistics, Assignment 1	
#I confirm that the attached is my own work, except where clearly indicated in the text.
#FUNCTION 1
my.rnorm <- function (n, mean=0, sd=1) {
    # Purpose :  Function that uses a uniform random number generator 
    #		 and creates a vector of pseudo-random normal deviates as an output.
    #
    # Inputs:
    # 	n: number of normally distributed deviates to return 
    # 	mean: mean of values to return where the default is 0
    # 	sd: standard deviation of values to return where the default is 1
    #
    # Outputs: 
    #	A vector "Xtransf" containing pseudo-random deviates from a normal distribution.
    #
    #Error handling 
    pass <- is.numeric(n)				#n can only be numeric value (not text)
    if (pass!=TRUE ) {					#If input arguments are non-numeric then error message printed
    stop("invalid arguments")}
    if (n%%1!=0 ) {
    stop("invalid arguments")} 			        #n must be input as an integer value only
    pass <- is.numeric(mean)				#mean can only be numeric value (not text)
    if (pass!=TRUE ) {					
    stop("invalid arguments")}
    pass <- is.numeric(sd)				#sd can only be numeric value (not text)
    if (pass!=TRUE ) {					
    stop("invalid arguments")}

    X <-vector(length=n)  				#Specifying that X is a vector.
    Xtransf <-vector(length=n) 			        #Specifying that Xtransf is a vector.
    for(j in 1:n) {
    for(i in 1:16) {
    U <- runif(i, min = 0, max = 1)}  		      	#Generates 16 random uniformly distributed deviates between 0 and 1
    X[j] <- (sum(U)-8)*sqrt(12/16) 			#Using Central Limit Theorem, calculates deviates with standard normal form
    Xtransf[j] <- ((X[j]*sd)+mean)			#Transforms deviates into values with mean µ and sd σ (also holds for default values)
    } 
    return(Xtransf)					#Returns a vector containing our required random normal deviates
}
#FUNCTION 2
my.rchisq<- function (n, df=1) {
    # Purpose :  Function that generates a vector of pseudo-random deviates from a chi-squared distribution,
    #           using a standard normal random variables.
    # Inputs:
    # 		n: number of normally distributed values to return 
    # 		df: degrees of freedom of the distribution where the default=1
    # Outputs 
    #		A vector Y of pseudo-random deviates with a chi-squared distribution
    #Error handling 
    pass1 <- is.numeric(n)				#n can only be numeric value (not text)
    pass2 <-is.numeric(df)			        #df can only be numeric value (not text)
    if (pass1!=TRUE ) {					#If input arguments are non-numeric then error message printed.
    stop("invalid arguments")}
    if (pass2!=TRUE ) {
    stop("invalid arguments")}
    if (n%%1!=0) {
    stop("invalid arguments")} 				#n must be input as an integer value
    if(df%%1!=0) {
    stop("invalid arguments")}				#df must be input as an integer value

    Z<-vector(length=df)				#Specifying that Z is a vector. Contains the value 'df' random standard normal deviates
    Y <-vector(length=n)			        #The vector Y contains pseudo-random chi-squared-distributed deviates. 
    for(i in 1:n) {
    for (j in 1:df) {					#This for loop accounts for non default degrees of freedom that might be input
    for(k in 1:16) {
    U <- runif(k, min = 0, max = 1)}  		        #Generates 16 random uniformly distributed deviates between 0 and 1
    Z[j] <- (sum(U)-8)*sqrt(12/16) 			#Calculation taken from the Central Limit Theorem, calculates 'df' independent deviates with standard normal form
    }	
    Y[i]<- sum((Z[j])^2) 			        #Using the algorithm for Theorem for chi-squared distributed deviates to output 'n' chi-squared deviates
    }
    return(Y)
}
#FUNCTION 3
my.rf<- function (n, df1=1, df2=1) {
    # Purpose :  Function that generates a vector of pseudo-random deviates from a F-distribution df1 and df2 degrees of freedom (user specified or default),
    #            using a two sets of randomly generated Chi-squared deviates
    # Inputs:
    #            n: number of deviates, with F distribution, to return 
    #            df1: degrees of freedom of the first distribution where the default=1
    #            df2: degrees of freedom of the second distribution where the default=1
    # Outputs: 
    #            A vector "F" of pseudo-random values from a F distribution
    #Error handling 
    pass1 <- is.numeric(n)				#n can only be numeric value (not text)
    pass2 <-is.numeric(df1)			        #df1 can only be numeric value (not text)
    pass3 <- is.numeric(df2)			        #df2 can only be numeric value (not text)
    if (pass1!=TRUE ) {					#If input arguments are non numeric then error message printed.
    stop("invalid arguments")}
    if (pass2!=TRUE ) {
    stop("invalid arguments")}
    if (pass3!=TRUE ) {
    stop("invalid arguments")}

    if (n%%1!=0) {
    stop("invalid arguments")} 				#n must be input as an integer value
    if(df1%%1!=0) {
    stop("invalid arguments")} 				#df1 must be input as an integer value
    if(df2%%1!=0) {
    stop("invalid arguments")} 				#df2 must be input as an integer value

    F <-vector(length=n)  			        #Specifying that F is a vector.

    for(j in 1:n) {				        
    U <- my.rchisq(1, df1)				#Employs use of our previously created my.rchisq function to calculate F distributed deviates
    V <- my.rchisq(1, df2)
    F[j] <- (U/(df1))/(V/(df2))                         #Calculates  F-distributed random deviates using Distribution theory theorem for F-distributed deviates       
    }
    return(F)
}
#ADDITIONAL FUNCTIONS (To test generated deviates)
# Function to test my.rnorm
 my.normtestfunct <-function (n, mean=0, sd=1) {
	# Purpose:	Testing that my.rnorm produces random deviates from a normal distribution by examining a qqplot and histogram
	# Inputs: 	
	#		n: Number of deviates with normal distribution to use in plot/histogram
	# Output: 	A qqplot and histogram containing the pseudo-random deviates generated by the function my.rnorm


	par(mfrow=c(1,2))					#Creates 1 row with two columns to display qqplot and histogram
	x <- my.rnorm(n, mean=0, sd=1)			
	qqnorm(x, main=paste("QQ normal n=", n))		#Visual inspection of this plot allows us to see if our deviates are normally distributed.
	qqline(x, col="red") 					
								
	hist(x)							
}
#COMMENTS
#There is one very important conclusion we can draw from this test - the greater the value of n the closer the deviates are to a normal deviation - which we would expect.
#If the deviates have a normal distribution there should be clear linearity in the plot, 
#the points should not have a curved shape and should be non-skewed. There should be no major
#deviations from the line in red.
#If our deviates are non-normally distributed then the histogram will not be bell shaped. Signs
#of normality include a bell shape with the highest frequency in the center of the distribution.
#If the histogram and qqplot show a normal distribution then our function my.rnorm has been sucessful in its purpose
# Function to test my.rchisq
my.chisqtestfunct <- function(n, df) {
     # Purpose: Plotting the deviates generated by my own function my.rchisq and examining whether their plots
     # compare similarly with the standard curve characteristics of the chi-squared distribution	
     # Inputs:
     #		n: Number of deviates generated with chi-squared distribution
     #		df: degrees of freedom of the distribution		
     # Outputs:	Three distribution curves with varying degrees of freedom
     
     x <- my.rchisq(n, df)					#assigning my randomly generated deviates to x
     curve( dchisq(x, df=df), from = 0, to = 100, add=TRUE)	#plotting the distribution curves from 0 to 100 and adding them on top of one another
}

#COMMENTS
#I ran this testing function three times and used the values df=1, 2, 5
#I also used the value n=25 as a standard for the three plots
#General properties of a chi-squared curve are that: if df>2 = bell shaped curve, if df=2 = Distribution is L shaped with maximum ordinate at 0,
#and if df<2 (>0) then distribution is L shaped with infinite ordinate at the origin.
#If the plots show the above characteristics it is a good test of whether the random deviates have a chi squared distrbution, meaning our function my.rchisq was sucessful
#Function to test my.rf
my.rftestfunct <- function(n, df1, df2) {
     # Purpose: Plotting a Empirical cdf of the deviates generated by my own function my.rchisq and examining
     #          if the plot compares with the standard empirical cdf of the default function rchisq, that already exists in R
     #
     # Inputs:
     #		n: Number of deviates generated with F distribution
     #		df1: degrees of freedom of the numerator 
     #		df2: degrees of freedom of the denominator
     # Outputs:	Emperical cdf curves of my.rchisq and rchisq
     

     	par(mfrow=c(1,2))							#Creates 1 row with two columns to display qqplot and histogram				
        x1 <- my.rf(n, df1, df2)						#assigning my randomly generated deviates to x1
	y1 <- rf(n, df1, df2)							#assigning randomly generated deviates (that we already know have an F distribution) to y1
 
	x2 <- ecdf(x1)								#Computes an empirical cumulative distribution function for our deviates generated by my.rf
	y2 <- ecdf(y1)								#Computes an empirical cumulative distribution function for deviates generated by rf							
	plot(x2, xlab= 'Sample quantiles of my.rf', ylab ='', main = 'Empirical cdf distribution of my.rf')	#Plots the two cdf curves side by side so we can compare them
	plot(y2, xlab= 'Sample quantiles of rf', ylab ='', main = 'Empirical cdf distribution of rf')
}
#COMMENTS
#The two curves should resemble one another if the function I created (my.rf) works well since we know the default function in R does produce pseudo-random F-distributed deviates. 
#For an D distribution the cdf vaguely resemble an exponential curve
#Note if the degrees of freedom are very large then the curve becomes almost vertical.