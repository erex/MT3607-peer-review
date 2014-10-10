#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n, mean=0, sd=1) {
#Function to return pseudo-random values from a normal distribution.
#Inputs: n - a scalar integer > 0, mean - a scalar double, sd - a scalar double > 0.
#Outputs: x - a vector of n scalar doubles.

	#Stops the function and reports an error if an input is incorrect.
	if (n == as.integer(n) && length(n) == 1 && n > 0 && is.numeric(n) == TRUE) {} else {stop("invalid arguments")}
	if (mean == as.double(mean) && length(mean) == 1 && is.numeric(mean) == TRUE) {} else {stop("invalid arguments")}
	if (sd == as.double(sd) && length(sd) == 1 && sd > 0 && is.numeric(sd) == TRUE) {} else {stop("invalid arguments")}

	x <- double(n)
				
	#For loop generates n normal(0,1) random values.
	for (i in 1:n) {

		u <- runif(16)
		
		#Transforms the uniform values into a single normal(0,1) random value.
		x[i] <- (sum(u)-8)*sqrt(12/16)
	}

	#Transforms the normal(0,1) values into normal(mean,sd) values.
	x <- x*sd+mean

	#Returns the vector of n normal(mean,sd) values.		
	return(x)
}




my.rchisq <- function(n, df=1) {
#Function to return pseudo-random values from a chi-squared distribution.
#Inputs: n - a scalar integer > 0, df - a scalar integer > 0.
#Outputs: chi - a vector of n scalar doubles.

	#Stops the function and reports an error if an input is incorrect.
	if (n == as.integer(n) && length(n) == 1 && n > 0 && is.numeric(n) == TRUE) {} else {stop("invalid arguments")}
	if (df == as.integer(df) && length(df) == 1 && df > 0 && is.numeric(df) == TRUE) {} else {stop("invalid arguments")}

	#Sources the other functions in file asmt1.r, but only if the file is in the current working directory.
	source("asmt1.r.txt")

	chi <- double(n)

	#For loop generates n chi-squared(df) random values.
	for (i in 1:n) {		

		#Generates df normal(0,1) random values by calling the 'my.rnorm' function, then squares them.
		z <- my.rnorm(df)^2

		#Transforms the normal(0,1)-squared values into a single chi-squared(df) value.
		chi[i] <- sum(z)
	}

	#Returns the vector of n chi-squared(df) values.
	return(chi)
}




my.rf <- function(n, df1=1, df2=1) {
#Function to return pseudo-random values from an F-distribution.
#Inputs: n - a scalar integer > 0, df1 - a scalar integer > 0, df2 - a scalar integer > 0.
#Outputs: x - a vector of n scalar doubles.

	#Stops the function and reports an error if an input is incorrect.
	if (n == as.integer(n) && length(n) == 1 && n > 0 && is.numeric(n) == TRUE) {} else {stop("invalid arguments")}
	if (df1 == as.integer(df1) && length(df1) == 1 && df1 > 0 && is.numeric(df1) == TRUE) {} else {stop("invalid arguments")}
	if (df2 == as.integer(df2) && length(df2) == 1 && df2 > 0 && is.numeric(df2) == TRUE) {} else {stop("invalid arguments")}

	#Sources the other functions in file asmt1.r, but only if the file is in the current working directory.
	source("asmt1.r.txt")

	F <- double(n)			

	#For loop generates n F(df1,df2) random values.
	for (i in 1:n) {

		#Generates a chisq(df1) random value by calling the 'my.rchisq' function.
		u <- my.rchisq(1, df1)

		#Generates a chisq(df2) random value by calling the 'my.rchisq' function.
		v <- my.rchisq(1, df2)

		#Transforms the chisq values into a single F(df1,df2) value.
		F[i] <- (u/df1)/(v/df2)	
	}

	#Returns the vector of n F(df1,df2) values.
	return(F)			
}




test.function <- function (n=NA, mean=NA, sd=NA, df=NA, df1=NA, df2=NA) {
	#Function to test function 'my.rnorm', 'my.rchisq' or 'my.rf'.
	#Inputs: Input variables for the function to be tested: may be valid or invalid.  Input variables for the other functions should be left as default NA.
	#Outputs: None.


	#Sources the other functions in file asmt1.r, but only if the file is in the current working directory.
	source("asmt1.r.txt")


	#Determines whether to test the specified function's responses to valid or invalid inputs.
	z <- integer(6)
	if (is.na(n)==FALSE) {if (n == as.integer(n) && length(n) == 1 && n > 0 && is.numeric(n) == TRUE) {z[1]<-0} else {z[1]<-1}} 
	if (is.na(mean)==FALSE) {if(mean == as.double(mean) && length(mean) == 1 && is.numeric(mean) == TRUE) {z[2]<-0} else {z[2]<-1}}
	if (is.na(sd)==FALSE) {if (sd == as.double(sd) && length(sd) == 1 && sd > 0 && is.numeric(sd) == TRUE) {z[3]<-0} else {z[3]<-1}}
	if (is.na(df)==FALSE) {if (df == as.integer(df) && length(df) == 1 && df > 0 && is.numeric(df) == TRUE) {z[4]<-0} else {z[4]<-1}}
	if (is.na(df1)==FALSE) {if (df1 == as.integer(df1) && length(df1) == 1 && df1 > 0 && is.numeric(df1) == TRUE) {z[5]<-0} else {z[5]<-1}}
	if (is.na(df2)==FALSE) {if (df2 == as.integer(df2) && length(df2) == 1 && df2 > 0 && is.numeric(df2) == TRUE) {z[6]<-0} else {z[6]<-1}}


	#If all inputs are valid, tests that the specified function produces the correct number of numeric responses.
	if(all(z==0)) {
		numeric.test(n=n, mean=mean, sd=sd, df=df, df1=df1, df2=df2)
	} else {

	#Tests that the specified function fails when given invalid inputs. 
		invalid.test(n=n, mean=mean, sd=sd, df=df, df1=df1, df2=df2)
	}

	invisible(NULL)
}




numeric.test <- function (n=NA, mean=NA, sd=NA, df=NA, df1=NA, df2=NA) {
#Function to test that the specified function produces n numeric values.
#Inputs: Valid input variables for the function to be tested.  Input variables for the other functions should be left as default NA.
#Outputs: None.


	#Determines the function to be tested based on the inputs.
	if (is.na(df) && is.na(df1) &&is.na(df2)) {
		output <- my.rnorm(n, mean, sd)
		function.name <- "my.rnorm"

	} else if (is.na(mean) && is.na(sd) && is.na(df1) &&is.na(df2)) {
		output <- my.rchisq(n, df)
		function.name <- "my.rchisq"

	} else if (is.na(mean) && is.na(sd) &&is.na(df)) {
		output <- my.rf(n, df1, df2)
		function.name <- "my.rf"

	} else {stop("invalid arguments")}


	#Tests that the function produces n values.
	if(length(output)==n) {
		cat(function.name, "produces the correct amount of values ")} else {
		cat(function.name, "produces the wrong amount of values ")}


	#Tests that the function produces numeric values.
	if(is.numeric(output)==TRUE) {
		cat("and they are numeric values.", "\n")} else {
		cat("and they are not numeric values.", "\n")}


	invisible(NULL)
}




invalid.test <- function (n=NA, mean=NA, sd=NA, df=NA, df1=NA, df2=NA) {
#Function to test that the specified function stops and produces the correct error message when given invalid inputs.
#Inputs: Input variables for the function to be tested: one or more must be invalid.  Input variables for the other functions should be left as default NA.
#Outputs: None.


	#Determines the function to be tested based on the inputs.
	if (is.na(df) && is.na(df1) &&is.na(df2)) {
		tryCatch(my.rnorm(n, mean, sd), error=function(e) e)
		function.name <- "my.rnorm"

	} else if (is.na(mean) && is.na(sd) && is.na(df1) &&is.na(df2)) {
		tryCatch(my.rchisq(n, df), error=function(e) e)
		function.name <- "my.rchisq"

	} else if (is.na(mean) && is.na(sd) &&is.na(df)) {
		tryCatch(my.rf(n, df1, df2), error=function(e) e)
		function.name <- "my.rf"

	} else {stop("invalid arguments")}


	#Tests that the function stops and produces the correct error message if given an invalid input.
	if(geterrmessage()=="invalid arguments" && exists("output") == FALSE) {
		cat(function.name, "fails if given an invalid input and displays the correct error message.", "\n")
	} else {
		cat(function.name, "does not fail and display the correct error message if given an invalid input.", "\n")
	}


	invisible(NULL)
}



