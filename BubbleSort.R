# Bubble sort 
# This kind of sorting can also known as swap and sort. It first checks which one is greater from the first 2 indices 
# and if it then it swaps. This goes on until all the elements are sorted(ascending). There is a condition to check 
# if the elements are sorted before finishing all the iterations. So that we don't have to run all the loop redundantly.
# The second function bubble.sort1 is nothing but to pass 2 vectors instead of one and then sort. 

# Bubble sort
bubble.sort <- function(x)
{ # Start bubble.sort function
  n = length(x)                    # store the length of vector x in n
  for (i in n:1)                   # for loop to all the values of x; outer loop
  { # Start for loop i
    m = i                          # store the value of i in m to use it for another loop
    for (j in 1:(m-1))             # for loop to check once; inner loop
    { # Start for loop j
      if (x[j] > x[j+1])           # check if the first value is greater than 2nd. If it is 
      { # start if
        temp = x[j]                # then swap the values 
        x[j] = x[j+1]
        x[j+1] = temp
      } # end if
    } # end for loop j
    if( (all(diff(x) >= 0)) )      # Check if the vector is sorted for just the inner loop. If it is then break out. 
    { # start if
      break()
    } # end if
  } # end for loop i
  return(x)
} # end function bubble.sort

# Bubble sort1 with two vectors as input. 
bubble.sort1 <- function(x1,x2)
{
  x = c(x1,x2)
  n = length(x)
  for (i in n:1)
  {
    m = i
    for (j in 1:(m-1))
    {
      if (x[j] > x[j+1])
      {
        temp = x[j]
        x[j] = x[j+1]
        x[j+1] = temp
      }
    }
    if( (all(diff(x) >= 0)) )
    {
      break()
    }
  }
  return(x)
}