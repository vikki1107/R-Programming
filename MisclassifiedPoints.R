################################################################################################################
#    function name: detect.misclass
#         This function uses the k-means algorithm to form a cluster which is same as the classification
#         of the data point and then find out the ones that have been misclassified.
#   Input arguments:
#         1. class.v is a vector of classes
#         2. p is a matrix of the data points 
#                         
#   Output: There are three different outputs in the form of list
#         1. err.found is a boolean vector that is true if at least one data point has been misclassified
#            or false if there are no misclassifications. 
#         2. err.loc is a vector containing the indices of the rows of matrix p that are the misclassified
#         3. new.class is a vector containing the correct classifications for the misclassified data points
################################################################################################################
detect.misclass <- function(class.v, p)
{
  # Conditions to check if class.v and p has the right value
  if ( !(is.finite(class.v) && is.numeric(class.v) && length(class.v) > 1) ) 
  { stop("class.v should be finite, numeric, and should have atleast2 classes") }
  
  if ( !(is.finite(p) && is.numeric(p) && class(p) == "matrix" && nrow(p) > 1) ) 
  { stop("p shouuld be finite, numeric, matrix and should have atleast 2 rows") }
  
  if ( !(length(class.v) == nrow(p)) )
  { stop(("Length of class.v is different from total number of rows in p"))}
  
  # if ( !(length(unique(class.v)) == ncol(p)) )
  # { stop(("Number of attributes measured for each observation is not equal to total columns in p"))}
  # if ( !(all(unique(class.v) %in% c(1,2))) )
  # { warning("class.v has a class apart from 1 or 2.")}
  
  # Initialize the output variables
  new.class <- NULL
  err.loc <- NULL
  
  # Get class.old vector to compare
  class.old = rep(1,nrow(p))
  
  # Sort the class.v vector and then create a matrix with rows as number of unique class
  unique.sorted.class = sort(unique(class.v))
  c = matrix(1,nrow = length(unique.sorted.class),ncol = 2, byrow = T)
  
  # Create a data frame with nrow(p) and columns with number of unique class + 1 and change the colnames
  alldist = data.frame(matrix(0, nrow = nrow(p), ncol = length(unique.sorted.class)+1))
  colnames(alldist) = c(paste0("d", unique.sorted.class), "class")
  
  # Assign the class.v to class column of alldist data frame
  alldist$class = class.v
  
  # Begin while loop to check if class.old and alldist classes are same
  # If not then assign the alldist class to class.old and calculate the distance
  # from first point(column means) to all the points in p and store it in d1 column of dataframe
  # Similarly calculate the rest of the distances. 
  # After getting distances assign the class number based which ever distance is minimum
  while(all.equal(class.old,alldist$class)!=TRUE)
  { # while loop begin
    class.old = alldist$class
    
    for (j in 1:nrow(c))
    {
      c[j,] = colMeans(p[which(alldist$class == j),])
      
      for (i in 1:nrow(p))
      {
        alldist[,j][i] = sum((p[i,]-c[j,])^2)
      }
    }    
    
    alldist$class = apply(alldist[,1:(ncol(alldist)-1)],1,which.min)
  } # while loop end
  
  # Now check if there are error in classes calculated and given if there are then err.found is TRUE else FALSE
  # if its true then get all the locations and the new classes from alldist class column
  err.found = any(alldist$class != class.v)
  if (err.found == TRUE)
  {
    err.loc = which(alldist$class != class.v)
    new.class = alldist$class[err.loc]
  }
  # Return the output as list
  return(list('err.found' = err.found, 'err.loc' = err.loc, 'new.class' = new.class))
}