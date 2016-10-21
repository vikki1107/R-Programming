# Bubble sort 
# This kind of sorting can also known as swap and sort. It first checks which one is greater from the first 2 indices 
# and if it then it swaps. This goes on until all the elements are sorted(ascending). There is a condition to check 
# if the elements are sorted before finishing all the iterations. So that we don't have to run all the loop redundantly.
# The second function bubble.sort1 is nothing but to pass 2 vectors instead of one and then sort. 

# Bubble sort
bubble.sort <- function(x)
{
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

# Bubble sort1
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
