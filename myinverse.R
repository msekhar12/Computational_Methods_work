myinverse <- function(A)
{
  rows <- nrow(A)
  cols <- ncol(A)
  
  if (rows != cols)
  {
    return ("The inverse cannot be found for the given matrix")
  }
  
  
  #Finding the co-factor matrix
  
  B <- A
  
  for (i in 1:rows)
  {
    for (j in 1:cols)
    {
      B[i, j] <- (((-1)^(i+j)) * det(A[-i, -j]))
      
    }
  }
  
  A_INV <- t(B)/det(A)
  
  return(A_INV)
}