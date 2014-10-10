#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function (n, mean=1, sd=0) {
    mean<- "σ" 
    sd <- "μ"  
     { if(mean > 0 & sd > 0) {
       print(TRUE) 
     } else { 
       print(FALSE) 
     }
     } 
#if the mean and/ or the standard deviation of the input fall below zero return a statement that reads "FALSE"
#if the staement is FALSE the function prints END to display the end of the function. If TRUE is printed then the function will proceed. 
      if (FALSE){
        print ("Invalid arguments")
        x <- ("UND")
      } else {
       n <- runif(16, min= 1, max= 1000)
       #To obtain a list of normally distributed deviates
        sum.x <- sum(n)
        #Calculates the summation portion of the given equation.
        x <- ((sum.x - 8) * (sqrt(12/16)))
        print (x)
       }
}


my.rchisq <- function(x, df, n ){
    df <- 1
    n <- round(runif(1, min = 1,max = 1000)) 
#To find the degree of freedom (df) first the command runs a runif statement to chose a number between 1 and 1000. The runif statement has then been enclosed by the round command to ensure that the result is an integer since degrees of freedom can only be integers.  
     for (i in 1: length(n)) {
       chi <- (sum((x)^2)) 
     } 
#Taking the x that was found in my.rnorm and using that in my chisq function.
}  

my.rf <- function (chi, x, df1, df2){
  df1 <- round(runif (1, min=1, max= 1000))
  df2 <- round(runif (1, min=1, max= 1000))  
  if (df1 & df2 > 0) {
    print(TRUE)
    val <- 1
  } else {
    print ("Invalid arguments")
    val <- 0
  }
  if (val > 0)  {
      num <- (sum(x))^2
      numerator <- (num / df1)
      den <- sum(chi)
      denominator <- (den/ df2)
      f <- numerator/ denominator 
      print(f)
  }else {
    print ("Invalid arguments")
    f <- "Invalid arguments"
  }  
}


#source('asmt1.r')
#my.rnorm (mean = -5)
#  if (x <- "UND") {
#    print ("Pass")
#  } else {
#    print ("Fail")
#}
#Checking to see if my.rnorm function will return a value for x when a neagtive mean is input.
