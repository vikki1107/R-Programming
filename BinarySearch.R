# BINARY SEARCH

binary.search <- function(x,num)
{
  low = 1; high = length(x); mid = (low + high) %/% 2
  
  for (i in 1:length(x))
  {
    while ( (low < high) && (num != x[i]))
    {
      if (num > x[i])
      {
        low = mid + 1
      }
      else
      {
        high = mid -1
      }
      mid = (low + high) %/% 2
    }
    if (num == x[i])
    {
      print(i)
    }
  }
}