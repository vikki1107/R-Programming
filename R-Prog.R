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


