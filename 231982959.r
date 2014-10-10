
#I confirm that the attached is my own work, except where clearly indicated in the text.


my.rnorm = function(n,mean=0,sd=1)
{
i=1
X=rep(0,n)  						#Creates vector x with n zero entries
In=as.integer(n) 					#Let In equal the rounded integer value of n
	if ( n<=0 || abs(n-(as.integer(n)))>0 || sd<0)  #Conditions for stopping function
	{
	stop("invalid arguments")
	}
		while(i<=n)                             #While loop generating random normal deviates
		{
		U=runif(16,0,1)
		X[i]=((sum(U))-8)*sqrt(12/16)		#Places each deviate in [i]th entry to vector x
		i=i+1
		}
Z=((sd)*(X)+(mean))					#Standard deviation and mean modifiers
print(Z)
}






my.rnorm.t = function(n,mean=0,sd=1)			#Creates normal random deviates with mean=0 and sd=1 but does not print,
{							#(only difference from my.rnom) makes the output of test.my.rnorm much
i=1							#cleaner, otherwise test.my.rnorm would print n normal deviates each time
X=rep(0,n)						#the test is conducted.
In=as.integer(n)
	if ( n<=0 || abs(n-(as.integer(n)))>0 || sd<0)
	{
	stop("invalid arguments")
	}
		while(i<=n)
		{
		U=runif(16,0,1)
		X[i]=((sum(U))-8)*sqrt(12/16)
		i=i+1
		}
Z=((sd)*(X)+(mean))
}
test.my.rnorm = function(n,m)				#Function that takes the "normal" deviates from my.rnorm.t and prints	
{							#the average of m calculated stanard deviations and means. Should my function above	
i=1							#be succsesful in generating normal deivates, we should expect the average mean to be
A=rep(0,m)						#zero and the average standard deviation to be one. The 2 arguments of test.my.rnorm are
B=rep(0,m)						#m and n, m is the number of trials to do, and n is the number of "normal" deviates generated
	while(i<=m)					#in each trail.If my.rnorm is succsessful in producing normal deviates we would expect
	{						#test.my.rnorm to produce 2 numbers, the first the mean, and the second the stanard deviation,
	A[i]=sd(my.rnorm.t(n))				#both being very close to the actual mean and standard deviation, supporting evidence than my.rnorm works.
	B[i]=mean(my.rnorm.t(n))			
	i=i+1
	}
print(mean(A))
print(mean(B))
}







my.rchisq=function(n,df=1)
{
	if (n<1 || abs(n-(as.integer(n)))>0 || df<0 || abs(df-(as.integer(df)))>0)	#Error conditions
	{
	stop("invalid arguments")
	}
k=1
X2=rep(0,n)
		while (k<=n)								#There are 2 while loops, one generating normals, the other X2, from the normals.		
		{
		i=1
		Z=rep(0,df)
				while (i<=df)
				{
				U=runif(16,0,1)
				Z[i]=((sum(U))-8)*sqrt(12/16)				#Generating df many random normal deviates
				i=i+1
				}
		Zsq=Z^2									#Converting the normal deviates to Chisq.
		X2[k]=(sum(Zsq))
		k=k+1
		}
print(X2)										#Prints n many X2 deviates.
}




my.rchisq.t=function(n,df=1)								#Similar as before, this function is exactly the same as my.rchisq, except it doesnt print results.
{
	if (n<1 || abs(n-(as.integer(n)))>0 || df<0 || abs(df-(as.integer(df)))>0)
	{
	stop("invalid arguments")
	}
k=1
X2=rep(0,n)
		while (k<=n)
		{
		i=1
		Z=rep(0,df)
				while (i<=df)
				{
				U=runif(16,0,1)
				Z[i]=((sum(U))-8)*sqrt(12/16)
				i=i+1
				}
		Zsq=Z^2
		X2[k]=(sum(Zsq))
		k=k+1
		}
X2
}
test.my.rchisq=function(n,m=10,df=10)							#My test function takes the sum of df many chisq deviates, and compares my.rchisq against rchisq. If my funcion					
{											#works, we should expect the average difference of the two to be zero. There are 3 arguments, we can do n trails 	
i=1											#of m many chisq deviates with df degrees of freedom.
T=rep(0,n)
		while(i<=n)
		{
		T[i]=((sum(my.rchisq.t(m,df)))-(sum(rchisq(m,df))))
		i=i+1
		}
print((mean(T)))
}




my.rf=function(n,df1=1,df2=1)
{
	if (n<1 || abs(n-(as.integer(n)))>0 || df1<0 || abs(df1-(as.integer(df1)))>0 || df2<0 || abs(df2-(as.integer(df2)))>0)
	{													 	                  #Error conditions
	stop("invalid arguments")
	}
k=1
F=rep(0,n)
		while(k<=n)							#While loop generating the F deviates
		{
		i=1
		X=rep(0,df1)
					while(i<=df1)				#While loop generating the X normals (later to be X2 and then U)
					{
					A=runif(16,0,1)
					X[i]=((sum(A))-8)*sqrt(12/16)
					i=i+1
					}
		j=1
		Y=rep(0,df2)
					while(j<=df2)				#While loop generating the Y normals (later to be Y2 and then V)
					{
					B=runif(16,0,1)
					Y[j]=((sum(B))-8)*sqrt(12/16)
					j=j+1
					}
		X2=X^2
		Y2=Y^2
		U=sum(X2)
		V=sum(Y2)
		F[k]=((U/df1)/(V/df2))						#Here the [i]th F deviate is calculated.
		k=k+1
		}
print(F)
}




my.rf.t=function(n,df1=1,df2=1)        						#Again, this function is the same as my.rf except it doesnt print results, this makes test.my.rf more user friendly.						
{
	if (n<1 || abs(n-(as.integer(n)))>0 || df1<0 || abs(df1-(as.integer(df1)))>0 || df2<0 || abs(df2-(as.integer(df2)))>0)
	{															 	
	stop("invalid arguments")
	}
k=1
F=rep(0,n)
		while(k<=n)							
		{
		i=1
		X=rep(0,df1)
					while(i<=df1)				
					{
					A=runif(16,0,1)
					X[i]=((sum(A))-8)*sqrt(12/16)
					i=i+1
					}
		j=1
		Y=rep(0,df2)
					while(j<=df2)				
					{
					B=runif(16,0,1)
					Y[j]=((sum(B))-8)*sqrt(12/16)
					j=j+1
					}
		X2=X^2
		Y2=Y^2
		U=sum(X2)
		V=sum(Y2)
		F[k]=((U/df1)/(V/df2))						
		k=k+1
		}
F
}
test.my.rf=function(n,m=10,df1=3,df2=3)  					 #If my.rf truely does make F distrubtued numbers then the mean should be df2/(df2-2)
{										 #test.my.rf calculates the difference in the mean of my.rf.t and df2/(df2-2). It does this n times, for m deviates, with df1=df1 and df2=df2.
i=1										 #It then calculates the average difference in n trails. We should expect the output of this function to be zero, or close to zero, if 
T=rep(0,n)									 #my.rf does indeed make F distributed deviates.
		while(i<=n)
		{
		meanx=mean(my.rf.t(m,df1,df2))
		meanH=(df2)/((df2)-2)
		T[i]=meanx-meanH
		i=i+1
		}
mean(T)
}


