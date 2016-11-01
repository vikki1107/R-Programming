
name = "Vivek Limbavali"

############################################################################################################################
# in.circle function distinguishes the points that are inside the circle from that are outside in a two dimensional plane. #
# Input arguments:                                                                                                         #
#          1. n points with xy coordinaters(which is nothing but a matrix of nX2)                                          #
#          2. Coordinates to the centre of the circle(which is nothing but a vector of length 2)                           #
#          3. A Radius(which is nothing but a single numeric value)                                                        #
# Output:                                                                                                                  #
#          1. A plot of circle and points(with different colors) on a 2D plane                                             #
#          2. xy coordinates of the points that are inside the circle.                                                     #
#                                                                                                                          #
# The way this function works is, it first checks if all the inputs arguments are supposed to be what they are.            #
# For ex: for an input arg pts, the function will first check if its a matrix by checking the class and then checks        #
# if the matrix has atleast one row and two column. Similarly it also checks if the center is just one point with one      #
# xy coordinate and a radius is numeric value.                                                                             #
# After completing the checks the function then plots a circle on a new graphic device with radius r and centre cntr.      #
# Next by calculating the euclidian distance from centre to each points on a plane, it check if the value is less than     #
# or equal to radius, if it is then it marks them inside the circle on plot with red color and if the distance is greater  #
# than radius then it will mark it outside the circle with blue color. It also store the points that are inside the circle #  
# in a matrix in.pts to return. If there are no points in a circle then the function will return a NULL value.             #
############################################################################################################################

in.circle <- function(pts,cntr,r)
{ # start function in.circle
  
  # Conditions to check the input arguments. 
  if ( !(class(pts) == "matrix" && nrow(pts) >= 1 && ncol(pts) == 2) ) 
  { stop("Either pts is not a matrix or it is not of dimension nX2") }
  if ( !(is.numeric(cntr) && length(cntr) == 2) ) 
  { stop("Either the center is not numeric or it is not a vector of length 2") }
  if ( !(is.numeric(r) && r >= 1 && length(r) == 1) ) 
  { stop("Either the radius is not numeric or it is not greater than 1 or it is a vector of length greater than 1") }
  
  # All the lines upto in.pts = NULL (i.e from line 37 to 51) are to draw a circle with centre cntr and radius r
  # Calculate point x and y using the angle theta. 
  theta = seq(0,2*pi,length = 2000)
  x = r*cos(theta)
  y = r*sin(theta)
  
  # Get x and y limits to use it as bounday while drawing circle
  # This keeps the circle in shape.
  xlimits = c( min(pts[,1],(cntr[1]-r))-1, max(pts[,1],(cntr[1]+r))+1 )
  ylimits = c( min(pts[,2],(cntr[2]-r))-1, max(pts[,2],(cntr[2]+r))+1 )
  boundary = c(min(xlimits,ylimits), max(xlimits,ylimits))
  
  x11()
  plot( cntr[1]+x, cntr[2]+y, type = 'l', xlab = "X", ylab = "Y", 
        xlim = boundary, ylim = boundary)
  
  title( "Divide points using circle", sub = "Vivek Limbavali", 
         cex.main = 2,  col.main = "chocolate", 
         cex.sub = 1.5, col.sub = "chocolate" )
  
  r.sq = r^2                                                  # Calculate the square of radius to compare with distance
  in.pts = NULL                                               # Empty vector/matrix to store all the distances 
  
  for(i in 1:nrow(pts))
  { # start for loop to calculate distances
    distance.sq = sum((pts[i,] - cntr)^2)                     # calculate the distance but without square root
    if (distance.sq <= r.sq)                                          
    { # start if condition to check if distance is less than squared radius 
      in.pts = rbind(in.pts, pts[i,])                         # if its less then store the points in in.pts
      points(pts[i,1], pts[i,2], col = 'red')                 # plot points that are less than r^2 in red
    } # end if condition
    else
    { # start else
      points(pts[i,1], pts[i,2], col = 'blue')                # plot points that are greater than r^2 in blue
    } # end else
    
  } # end for loop to plot the points on graph
  return(in.pts)                                              # return the points that are inside the circle
} # end function in.circle