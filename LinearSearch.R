# LINEAR SEARCH
# A function that bins data. This is the kind of thing one does when making a histogram. We are given a 
# data vector x, and a vector containing the boundary of the bins. This vector is called bins. Here x is 
# a numeric data vector, and we want to bin the data. If bins has length m, then we return a vector of 
# length (m+1). 
# This function uses a linear search algorithm where it checks the position of first data value in all 
# the bins. Suppose say if the data value falls in first bin then this program/algorithm breaks and will 
# start with second data value. Suppose if the bin size is large and the data value falls in last bin then
# this algorithm will take more time and hence not much efficient. 

# A function that bins the data using linear search.
bin.data <- function(x,bins)
{ # start function bin.data
  # Below lines are to check if the bins are in ascending order
  if ( !(all(diff(bins) >= 0 ) ) ) { stop("bins vector is not in ascending order") }
  
  n = length(bins) + 1                           # length of the output vector
  z = numeric(n)                                 # output vector with n 0's
  bins = c(-Inf, bins, Inf)                      # append -Inf and Inf to bins to get the intervals for comparison
  
  for(i in 1:length(x))                                  
  { # start for loop
    for (j in 1:n)
    { # start for loop for bins
      if ((bins[j] < x[i]) && (x[i] <= bins[j+1])) # check if the first element of input vector falls in first bin 
      {                                          # boundary. If so then increment the value of that index by 1
        z[j] = z[j] + 1                          # If not then go to the second bin boundary and so on until all the bins  
        break()                                  # are checked. If the perfect bin is found then break the loop. 
      }                                          
    } # end for loop for bins
  } # end for loop
  return(z)                                        # return the output
} # end function bin.data