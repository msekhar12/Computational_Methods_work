
#find_first_and_divide:
#This function takes a vector as input. It finds the first non-zero element in the vector, and 
#divides the remaining elements with the first non-zero element. The position of the non-zero element
# is appended to the modified vector (after division), and the vector is returned

find_first_and_divide <- function(x)
{
  
  for( i in 1:length(x))
  {
    #if(x[i] == 1) return(data.frame(pos = i, newrow  = x))
    #if(x[i] !=0) return(data.frame(pos = i, newrow = (x/x[i])))
      
    #if(x[i] == 1) return(list(pos = i, newrow  = x))
    #if(x[i] !=0) return(list(pos = i, newrow = (x/x[i])))
    
    if(x[i] == 1) return(c(x,i))
    if(x[i] !=0) return( c((x/x[i]),i))
    
  }
  
  #return(list(pos=i, newrow = x))
   return(c(x,(i+1)))
}


row_echelon_form <- function(m)
  
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
    
  repeat{
 #     print("at start")
#      print(m)
      
    m_mod <- t(apply(m,1,find_first_and_divide))
 #   print(m_mod)
    #print (m_mod)
    operation_tracker[,1] <- m_mod[,(m_cols+1)]
    operation_tracker <- operation_tracker[order(operation_tracker[,1]),]
#    print(operation_tracker)
    
    m_mod <- m_mod[operation_tracker[,2],-(m_cols+1)]
 #   print(m_mod)
    
    #Check if all the columns (except the last column) are zeros, and alert 
    if(operation_tracker[m_rows,1] == m_cols) {return(list(msg="NO solution found",m_mod))}
    
    #Check if all the rows are zeros, and alert that infinite possible solutions
    if(operation_tracker[m_rows,1] == (m_cols + 1)) return(list(msg="Infinite number of solutions possible",m_mod))
 
 #print(operation_tracker[-m_rows,1])
 #print(operation_tracker[-1,1])
 
  if (any((operation_tracker[-m_rows,1] - operation_tracker[-1,1]) == 0))
  {
  #  print("in loop")
   if(m_rows >= 2)
    {
    i <- 1
    j <- 2
    
    repeat {
        if (j > m_rows) break
        if(operation_tracker[i,1] == operation_tracker[(j),1])
         {
          m_mod[operation_tracker[j,2],] <- ( m_mod[operation_tracker[j,2],] - m_mod[operation_tracker[i,2],] )
          j <- (j+1)
          
         }
         else {
           i <- (j)
           j <- (j + 1)
         }
        }
    
    }
  }
  
  else return(list(original = original, row_echelon=m_mod))
  
   m <- m_mod
  }
 
  
  
  } 
  else return("Incorrect input. Supply matrix as input")
  
}