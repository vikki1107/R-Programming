# MERGE SORT
# Merge sort is nothing but to merge two vectors into one sorted vector. The logic here is to compare the first
# elements of both the vectors and store the smallest one in a new vector. Then increment the counter for the 
# vector from which the value was stored and then perform the comparision again. This happens until all the 
# elements of one vector completes. And suppose if the other vector still has come length after the comparision
# then add the remaining elements to the output vector. 

# A function that merges two already sorted vectors and then sorts that vector.
merge.sort <- function(in1,in2)
{                                                 # begin function to sort
  # Below two lines are to check if the vectors are numeric and if they are sorted. 
  if ( !(is.numeric(in1) && is.numeric(in2) ) ) { stop("either one of the vector is not numeric") }
  if ( !(all(diff(in1) >= 0 ) && all(diff(in2) >= 0 ) ) ) { stop("either one of the vector is not in ascending order") }
  
  z = numeric(length(in1) + length(in2))          # this creates a output vector of 0's 
  i = 1; j = 1; k = 1;                            # initialize pointers 
  
  while( i <= length(in1) && j <= length(in2))    
  { # while loop start
    if( in1[i] < in2[j])                          
    {                                             
      z[k] = in1[i]                               # if the first element of vector 1 is smaller then 
      i = i + 1                                   # add it to the vector z and increment pointer
    }
    else                                          
    {                                             # else add the first element of vector 2 and increment  
      z[k] = in2[j]                               # the counter
      j = j + 1
    }
    k = k + 1                                     # increment the counter of output vector
  }
  
  while( i <= length(in1))                        # this while loop is for all the elements from vector1 that are 
  {                                               # not covered in first while loop
    z[k] = in1[i]
    i = i + 1
    k = k + 1
  }
  
  while( j <= length(in2))                        # this while loop is for all the elements from vector2 that are
  {                                               # not covered in first while loop
    z[k] = in2[j]
    j = j + 1
    k = k + 1
  }
  return(z)                                       # return the output
}                                                 # end function merge.sort
