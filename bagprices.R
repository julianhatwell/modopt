#Problem Description of Bag Prices; 
#adapted from "Modern Optimization with R" by Cortez, Paulo.
source("functions.R")

D=5 # dimension (number of prices)
MaxPrice=1000
Dim=ceiling(log(MaxPrice,2)) # size of each price (=10)
size=D*Dim # total number of bits (=50)

intbin=function(x) # convert binary to integer
{ 
  sum(2^(which(rev(x==1))-1)) 
} # explained in Chapter 3 

bintbin=function(x) # convert binary to D prices
{ # note: D and Dim need to be set outside this function
  s=vector(length=D)
  for(i in 1:D) # convert x into s:
  { ini=(i-1)*Dim+1;end=ini+Dim-1
  s[i]=intbin(x[ini:end])
  }
  return(s)
}

bprofit=function(x) # profit for binary x
{ 
  s=bintbin(x)
  s=ifelse(s>MaxPrice,MaxPrice,s) # repair!
  f= profit(s) # maximization task!
  return(f)
}