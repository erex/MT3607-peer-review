#I can confirm that the attached is my own work, except where clearly indicated in the text.

#MT3607 Assignment 1

# The following functions have been designed to generate random deviates from different distributions
# Starting with a uniform distribution I first generate normally distributed values. 
# Using this code I then generate Chi Squared deviates and then from them F-distributed deviates

#The following function outputs normaly distributed values with default mean=0 and standard deviation=1
my.rnorm<-function(n,mean=0,sd=1){
  if (class(n)!='numeric') stop('Invalid Arguments')
  # The following line of code runs and error check to make sure that the number of values asked for is an integer as is is impossible to have half of a result
  # If the original n in the calling function is not an integer then the function will stop and print an error message
  # This works by taking the integer part of a number away from itself 
  # This way if it is an integer it should equal 0, if however it does not, we know that the original n was not an integer, so not a valid input
  if (n-as.integer(n)!=0) stop('Invalid Arguments')
  #the following 2 lines of code set elements into vectors to hold multiple values (list of numeric values)
  u<-numeric(16)
  m<-numeric(n)
  #we can now start the loop to repeat as many times that we need outputed
  for (j in 1:n){
    #This next internal loop will generate the 16 uniform values required for the transformation to normal
    for (i in 1:16) {
      {u[i]<-runif(1)
      x<-c(u[1],u[2],u[3],u[4],u[5],u[6],u[7],u[8],u[9],u[10],u[11],u[12],u[13],u[14],u[15],u[16])
      }
  # We then sum these 16 values
      X<-sum(x)
    }
# Now these n values that we have generated are manipulated to create normaly distributed values using the algorithm given
  m[j]<-((X-8)*sqrt(12/16))*sd+mean
  j=j+1}
# This now prints the values generated into a list
  return(m)
}



# The following function generates n Chi Squared distributed values from the normal distribution, with degrees of freedom defaulted at 1
my.rchisq<-function(n,df=1){
  # The next four lines of code are error checks
  # The first two check the typr of data that is entered. It will only accept numerical values, not letters or any other type of data
  # They check that the initial arguments inputed into the function are integers
  # If they are not integers then the function will stop and an error message will appear
  if (class(n)!='numeric') stop('Invalid Arguments')
  if (class(df)!='numeric')stop('Invalid Arguments')
  if (n-as.integer(n)!=0) stop('Invalid Arguments')
  if (df-as.integer(df)!=0) stop('Invalid Arguments')
  # Sets the vector b up to hold numerical values
  b<-numeric(n)
  # Starts the loop to repeat to genarate the number of values that we need
  for (k in 1:n){
    # This generates the normal values, squares them and then sums them to generate the Chi squared values
    a<-my.rnorm(df)
    b[k]<-sum(a^2)
    # The next line of code checks that the outputs are positive which they should be in a Chi Squared distribution
    # If they are not, it stops the function and prints an error message
    if (b[k]<0) stop("Impossible outputs")
  }
  # This outputs all of the generated Chi Squared values into a list
  return(b)
}



# This function takes Chi Squared distributed values and turns them into F-distributred deviates, where the default degrees of freedom are 1 for both
my.rf<-function(n,df1=1,df2=1){
  if (class(n)!='numeric') stop('Invalid Arguments')
  if (class(df1)!='numeric') stop('Invalid Arguments')
  if (class(df2)!='numeric') stop('Invalid Arguments')
  if (n-as.integer(n)!=0) stop('Invalid Arguments')
  if (df1-as.integer(df1)!=0) stop('Invalid Arguments')
  if (df2-as.integer(df2)!=0) stop('Invalid Arguments')
  # This creates 3 vectors, z, c1 and c2 to hold numerical values
  z<-numeric(n)
  c1<-numeric(1)
  c2<-numeric(1)
  #This starts the loop that will repeat itself generating a new F-distributed deviate each time
  for (h in 1:n){
    # The next two lines of code each generate a different Chi Squared variable
    c1<-my.rchisq(1,df1)
    c2<-my.rchisq(1,df2)
    # This line takes these Chi Squared deviates and manipulates them into a F-distribution deviate
    z[h]<-(c1/df1)/(c2/df2)
    # The next line of code checks that the outputs are positive which they should be in a F distribution
    # if they are not, it stops the function and prints an error message
    if (z[h]<0) stop('Impossible Outputs')
  }
  return(z)
}


# TEST FUNCTIONS

# The following 3 functions test my other functions to make sure that they perform as they are meant to
# Within the original code for generating Chi Squared and F-distributed deviates I included lines of code to check 
# that the outputs are positive as it is impossible to get negative values from these distributions
# This is because they are generated using square numbers which must be positive 

# This next function checks the function my.rnorm and makes sure it outputs what it is meant to
check.rnorm<-function(n){
  x<-my.rnorm(n)
  # It checks to see if the function has the correct number of outputs that was originally asked for
  if (length(x)==n) print('Correct length')
  # If the length is not correct then it will print an error message to say that it is the incorrect length
  if (length(x)!=n) stop('Incorrect length')
  # It checks to see if the funtion outputs numerical variables which it should.
  if (is.numeric(x)) print('All numerical values')
  # If the ouputs are not numbers then the following line of code stops the function and prints out an error statement
  if (is.numeric(x)!=TRUE) stop('Incorrect data type')
}


# This next function checks the function my.rchisq and makes sure it outputs what it is meant to 
check.rchisq<-function(n,df=1){
  x<-my.rchisq(n,df=1)
  # It checks to see if the function has the correct number of outputs that was originally asked for 
  if (length(x)==n) print('Correct length')
  # If the length is not correct then it will print an error message to say that it is the incorrect length
  if (length(x)!=n) stop('Incorrect length')
  # It checks to see if the function outputs numerical variables which it should
  if (is.numeric(x)) print('All numerical values')
  # If the outputs are not numbers then the following line of code stops the function and prints out an error statement 
  if (is.numeric(x)!=TRUE) stop('Incorrect data type')
  # The next loop just checks that none of the outputs are negative as they cannot be in the Chi Squared distribution
  # I have already checked for this error in the original function, however this will double check it
  for (i in 1:n){
    if (x[i]<0) stop('Impossible outputs - cannot be negative')
  }
}


# This next function checks the function my.rf and makes sure it outputs what it is meant to 
check.rf<-function(n,df1=1,df2=1){
  x<-my.rf(n,df1=1,df2=1)
  # It checks to see if the function has the correct number of outputs that was originally asked for 
  if (length(x)==n) print('Correct length')
  # if the length is not correct then it will print an error message to say that it is teh incorrect length
  if (length(x)!=n) stop('Incorrect length')
  # It checks to see if the function outputs numerical variables which it should 
  if (is.numeric(x)) print('All numerical values')
  # If the outputs are not numbers then the following line of code stops the function and prints out an error statement
  if (is.numeric(x)!=TRUE) stop('Incorrect data type')
  # The next loop just checks that none of the outputs are negative as they cannot be in the F-distribution 
  # I have already checked for this error in the original function, however this will double check it
  for (i in 1:n){
    if (x[i]<0) stop('Impossible outputs - cannot be negative')
  }
}