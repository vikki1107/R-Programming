####################################################################################################################################
# in.circle function distinguishes the points that are inside the circle from that are outside in a two dimensional plane.         #
# The function takes 3 input arguments: 1. n points with xy coordinaters(which is nothing but a matrix of nX2)                     #
#                                       2. Coordinates to the centre of the circle(which is nothing but a vector of length 2)      #
#                                       3. A Radius(which is nothing but a single numeric value)                                   #
# The function returns two things: 1. A plot of circle and points(with different colors) on a 2D plane                             #
#                                  2. xy coordinates of the points that are inside the circle.                                     #
# The way this function works is, it first checks if all the inputs arguments are supposed to be what they are. For ex: for a      #
# input arg pts, the function will first check if its a matrix by checking the class and then checks if the matrix has atleast     #
# one row and two column. Similarly it also checks if the center is just one point with one xy coordinate and a radius is numeric  #
# value.                                                                                                                           #
# After completing the checks the function then plots a circle on a new graphic device with radius r and centre cntr.              #
# Next it calculates the euclidian distance from centre to each points on a plane and stores it in a vector. To do so it will      #
# make use of another function eucli.dist which takes two arguments: 1. xy coordinates of point and                                #
#                                                                    2. xy coordinates of centre                                   #
# Once it has all the distances it will then check the values that are less than or equal to radius and mark them on plot which    #
# has a circle with red color and the ones that are greater than radius with blue. It also store the points that are inside the    #
# circle in local variables in.x and in.y then bind them into a matrix in.pts to return. If there are no points in a circle then   #  
# the function will return a NULL value.                                                                                           #
####################################################################################################################################

# Function that calculates the euclidian distance
eucli.dist <- function(x1,x2) 
{ # start function eucli.dist
  sqrt( sum((x1-x2)^2) )
} # end function eucli.dist

in.circle <- function(pts,cntr,r)
{ # start function in.circle
  
  # Conditions to check the input arguments. 
  if ( !(class(pts) == "matrix" && nrow(pts) >= 1 && ncol(pts) == 2) ) 
  { stop("Either pts is not a matrix or it is not of dimension nX2") }
  if ( !(is.numeric(cntr) && length(cntr) == 2) ) 
  { stop("Either the center is not numeric or it is not a vector of length 2") }
  if ( !(is.numeric(r) && r >= 1 && length(r) == 1) ) 
  { stop("Either the radius is not numeric or it is not greater than 1 or it is a vector of length greater than 1") }
  
  # All the lines upto dist.pts.cntr = 0 is to draw a circle with centre cntr and radius r
  theta = seq(0,2*pi,length = 2000)
  x = r*cos(theta)
  y = r*sin(theta)
  
  x11()
  plot( cntr[1]+x, cntr[2]+y, type = 'l', xlab = "X", ylab = "Y",
        xlim = c( min(pts[,1],(cntr[1]-r))-1, max(pts[,1],(cntr[1]+r))+1 ), 
        ylim = c( min(pts[,2],(cntr[2]-r))-1, max(pts[,2],(cntr[2]+r))+1 ) )
  
  title( "Divide points using circle", sub = "Vivek Limbavali", 
         cex.main = 2,  col.main = "chocolate", 
         cex.sub = 1.5, col.sub = "chocolate" )
  
  dist.pts.cntr = 0                                  # Empty vector to store all the distances 
  for(i in 1:nrow(pts))
  { # start for loop to calculate distances
    dist.pts.cntr[i] <- eucli.dist(pts[i,],cntr)     # call eucli.dist function to calculate the distances
  } # end for loop to calculate distances
  
  in.x = 0; in.y = 0; j = 1; in.pts = NULL           # Empty vector in.x, in.y for x & y; j is counter; in.pts is NULL matrix 
  for (i in 1:length(dist.pts.cntr))
  { # Start for loop to plot the points on graph
    if (dist.pts.cntr[i] <= r)                       # If the distance is less than or equal to radius then 
    { # start if condition
      in.x[j] = pts[i,1]                             # Store the x coordinates into in.x
      in.y[j] = pts[i,2]                             # Store the y coordinates into in.y
      in.pts = cbind(in.x,in.y)                      # bind in.x & in.y to form matrix in.pts with nX2 dimension
      points(pts[i,1], pts[i,2], col = 'red')        # plot points that are inside with red
      j = j + 1                                      # increment the index of in.x and in.y to store next values
    } # end if condition
    else
    { # start else
      points(pts[i,1], pts[i,2], col = 'blue')       # plot points that are outside with blue  
    } # end else
  } # end for loop to plot the points on graph
  
  if (!is.null(in.pts))                              
  { # start if condition to rename the colnames
    colnames(in.pts) = c("x","y")                    # Rename the column names from in.x & in.y to x and y
  } # end if condition to rename the colnames
  
  return(in.pts)                                     # return the points that are inside the circle
} # end function in.circle