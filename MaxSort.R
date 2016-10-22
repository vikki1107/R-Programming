# MAX SORT
# This program takes a maximum value from a vector and stores it in a new one. After the store it removes
# the stored value from the vector passed to the function within it and performs the sort. 

max.sort <- function(x)
{
  n = length(x)
  y = numeric(n)
  for (i in n:1)
  {
    y[i] = max(x)
    x = x[-which(x==y[i])]
  }
  return(y)
}
