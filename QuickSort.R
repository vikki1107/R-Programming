# QUICK SORT
# This function basically checks if the data has a length of more than 1. If so then it will perform the sorting algorithm
# It stores the first element into pointer p and then use that pointer to compare all the values that are less or greater. 
# First it takes the values that are less than pointer p and pass it to function again to check the values that are less or 
# greater. This happens until we get a vector of length 1. This low value is stored in left and the p value is stored in 
# mid. Now all the values greater than the pointer are passed to function again to check the values left and right to it. 
# So suppose if there are values that are on left then the values are stored in left. But this is all taking place within 
# right (basically using different stack memory). 
# Finally it combines the left mid and right. First iteration would be from the values within right. That is high[left,mid,right]
# then it takes this high and combines it with left and mid. 
# To understand this you can use print statements in between. And this function is a good example of recursive functions. 

# A function that quick sorts a vector
quick.sort <- function(x) 
{ # quick.sort function start
  if(length(x) > 1)                # check if the length of the vector is greater than 1
  {                                # This check is performed to see if the vector needs any sorting. 
    p <- x[1]                      # assigning first value to a pointer p 
    left <- quick.sort(x[x < p])   # Run quick.sort function recursively for all the values less than pointer p.
    mid <- x[x == p]               # Store the pointer p into mid 
    right <- quick.sort(x[x > p])  # Run quick.sort function recursively for all the values greater than pointer p. 
    c(left, mid, right)            # finally combine left, mid and right
  }
  else x                           # else condition to store the single value
} # quick.sort function end