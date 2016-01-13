#find_first_and_divide:
#This function takes a vector as input. It finds the first non-zero element in the vector, and 
#divides the remaining elements with the first non-zero element. The position of the non-zero element
# is appended to the modified vector (after division), the divisor is also appended to the modified vector, and the modified vector is returned


find_first_and_divide <- function(x)
{
  
  
  
  for( i in 1:length(x))
  {
    
    if(x[i] == 1) return(c(x,i,1))
    divisor <- x[i]
    if(x[i] !=0) return( c((x/x[i]),i,divisor))
    
  }
  
  return(c(x,(i+1),1))
}


## get_u_matrix will apply the elementary row operations on the input matrix, and returns a list with modified matrix (U) or a message, if the modification fails.
#  The function can apply all the row operations, except the row interchange operation. If rows have to be interchanged, then it is not possible to convert a square matrix into LU decomposition


get_LU_matrix <- function(m)
  
{
  if(is.matrix(m))
  {
    #find the first non-zero element in each row, and divide the other elements by the first non-zero element
    original <- m
    m_rows <- nrow(m)
    m_cols <- ncol(m)
    
    
    m_mod <- matrix(nrow = m_rows, ncol =( m_cols+1))
    operation_tracker <- matrix(nrow = m_rows, ncol = 2)
    operation_tracker[,2] <- 1:m_rows
    
    if(m_rows != m_cols) return(list(msg="The given matrix is not a square matrix. LU Decomposition is not possible", original = original))
    
    e_matrix <- diag(m_rows)
    L <- diag(m_rows)
    e_matrix_temp <- diag(m_rows)
    
    repeat{
      
      m_mod <- t(apply(m,1,find_first_and_divide))
      
      operation_tracker[,1] <- m_mod[,(m_cols+1)]
      
      for(i in 1:m_rows)
      { 
        #e_matrix is an identity matrix. As per the logic, we need to first divide the [i,i] element of e_matrix with m_mod[i,m_cols+2] (which is nothing but the first non-zero element of m)
        #and after dividing, we have to get the inverse of e_matrix. But since e_matrix is a diagonal matrix, the inverse of diagonal matrix is nothing but a matrix with 
        #reciprocal of its diagonal elements. This is applicable only if the diagonal elements are non-zeros. So e_matrix[i,i] element has to be just multiplied with m_mod[i,m_cols+2], at appropriate 
        #row/column position on the diagonal, to obtain the required inverse of e_matrix (elementary matrix)
        
        e_matrix[i,i] <- m_mod[i,(m_cols+2)]
        L <- L %*% e_matrix
        
        print("initial L")
        print(L)
        
        #instead of allocating a new e_matrix, I am just reverting the operation made on e_matrix, and thus making it as back to identity matrix.
        e_matrix[i,i] <- 1 
      }
      
      
      m_mod <- m_mod[,-(m_cols+1):-(m_cols+2)]
      print("m_mod initial")
      print(m_mod)
      
      
      
      
      #The first column of operation_tracker matrix has the position of the first non-zero elements in each row.
      #The second column of operation_tracker matrix has the row number
      
      #The LU Decomposition is NOT possible, if we have to interchange the rows in the elimination process. 
      #Checking if we have to interchange the rows. If yes, then alert that LU decomposition is not possible, else proceed further
      
      #We have to interchange the rows in the elimination process, whenever we have the first non-zero element in a row occurs in the column, which is greater than the corresponding row.
      
      #The second condition in the following if statement will make sure that we do not raise alarm, if any of the row(s) have all zeros
      if(any(operation_tracker[,1] > operation_tracker[,2] & operation_tracker[,1] <= m_cols)) 
        #      if(any(operation_tracker[,1] > operation_tracker[,2])) 
        return(list(msg="The given matrix is requires rows interchange in the elimination, and hence LU Decomposition is not possible. Reorder of rows might fix this problem", original = original, modified = m_mod))  
      
      print("stub-x")
      print(operation_tracker[-m_rows,1])
      print(operation_tracker[-1,1])
      
      print(operation_tracker[-m_rows,1] - operation_tracker[-1,1])
      
      
      if (any(operation_tracker[,1] < operation_tracker[,2]))
      {
        pending <- which(operation_tracker[,1] < operation_tracker[,2])
        print("pending")
        print(pending)
        for (k in 1:length(pending))
        {
          if(m_mod[operation_tracker[pending[k],1],operation_tracker[pending[k],1]] == 0) 
            return(list(msg="The given matrix is requires rows interchange in the elimination process, and hence LU Decomposition is not possible", original = original, modified = m_mod))  
          m_mod[pending[k],] <- (m_mod[pending[k],] - m_mod[operation_tracker[pending[k],1],])
          e_matrix[pending[k],] <- ( e_matrix[pending[k],] - e_matrix[operation_tracker[pending[k],1],] )
          e_matrix_temp <- solve(e_matrix)
          L <- L %*% e_matrix_temp
          
          #Reverting the change to the identity matrix, so that we can re-use it, instead of allocating again
          
          e_matrix[pending[k],] <- ( e_matrix[pending[k],] + e_matrix[operation_tracker[pending[k],1],] )
          
        }
      }
      
      else return(list(msg="The L and U matrices along with verification is here", U=m_mod, L=L, original=original, LU = L %*% m_mod, verification = all.equal(original, ( L %*% m_mod))))
      m <- m_mod
      
      
    }
    
    
    
  } 
  else return("Incorrect input. Supply matrix as input")
  
}
