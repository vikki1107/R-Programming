# Convolution function

conv <- function(x,y)
{
  n = length(x)
  m = length(y)
  z = numeric(n+m-1)
  z.x = c(rep(0,m-1),x)
  z.y = c(rev(y),rep(0,n-1))
  for (i in 1:length(z))
  {
    z[i] = sum(z.x*z.y)
    z.y = c(0,z.y[1:(n+m-2)])
  }
  return(z)
}
