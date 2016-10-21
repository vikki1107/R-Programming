# Random practice programs. All at one place.

rm(list = ls())

setwd('/mnt/Flex/CSUEB/Courses/F_STAT6260-RProgramming/')

a = c(4,11,-5,2,3,6)
m = length(a)
out = numeric(m)
for (k in 1:m)
{
  if (a[k] <0)
  {
    print(a[k])
    print("invalid")
    break
  } # end if
  else 
  {
    out[k] = sqrt(a[k])
    print(out[k])
  } # end else
}

x = c(5,10,15)
y = c(2,4,6,8,10,12)
n = length(y)
z = numeric(n)
for (i in 1:n)
{
  if (i < 4)
  {
    z[i] = x[i] + y[2*i]
  }
  else
  {
    z[i] = x[i-3] + y[2*(i-3)]
  }
}

bin.data1 <- function(x,bins)
{ # start function bin.data1
  # Below lines are to check if the bins are in ascending order
  if ( !(all(diff(bins) >= 0 ) ) ) { stop("bins vector is not in ascending order") }
  
  n = length(bins) + 1                           # length of the output vector
  z = numeric(n)                                 # output vector with n 0's
  bins = c(-Inf, bins, Inf)                      # append -Inf and Inf to bins to get the intervals for comparison
  
  for(i in x)                                  
  { # start for loop
    for (j in 1:n)
    { # start for loop for bins
      if ((bins[j] < i) && (i <= bins[j+1]))     # check if the first element of input vector falls in first bin 
      {                                          # boundary. If so then increment the value of that index by 1
        z[j] = z[j] + 1                          # If not then go to the second bin boundary and so on until all the bins  
      }                                          # are checked. This is linear search which will take a bit time. 
    } # end for loop for bins
  } # end for loop
  return(z)                                        # return the output
} # end function bin.data1


# max sort
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

