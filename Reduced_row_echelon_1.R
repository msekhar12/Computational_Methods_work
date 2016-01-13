
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
    
    print(m_rows)
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
      if(operation_tracker[m_rows,1] == m_cols) {return(list(msg="NO solution found",m_mod=m_mod,proceed_further=0))}
      
      #Check if all the rows are zeros, and alert that infinite possible solutions
      if(operation_tracker[m_rows,1] == (m_cols + 1)) return(list(msg="Infinite number of solutions possible",m_mod=m_mod,proceed_further=0))
      
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
      
      else return(list(msg="Single solution is possible", row_echelon=m_mod,proceed_further=1))
      #else return(m_mod))
      m <- m_mod
    }
    
    
    
  } 
  else return("Incorrect input. Supply matrix as input")
  
}

reduced_row_echelon_form <- function(m,b)
{
 
  if(is.matrix(m) & is.matrix(b))
  {
    if(nrow(m) != nrow(b)) return("The number of rows in the input matrices are not identical")
    original <- m
    m <- cbind(m,b)
    m_row_echelon <- row_echelon_form(m)
    m <- m_row_echelon$row_echelon
    print(m_row_echelon)
    proceed_further <- m_row_echelon$proceed_further
    if(proceed_further == 0) return(list(original_matrix=original,constraint_matrix = b, msg=m_row_echelon$msg, row_echelon_form=m))
    
    m_rows <- nrow(m)
    m_cols <- ncol(m)
    
    
    print(m_rows)
#    print(m_cols)
    
    for(i in (1:m_rows))
    {
      x <- which(m[i,-m_cols] !=0)
      x_l <- length(x)
      
      if(x_l > 1)
      {
        for(j in 2:x_l)
        {
          m[i,] <- (m[i,] - (m[x[j],] * m[i,x[j]]))
        }
      }
      
  
    }
    #print(m)
return(list(original_matrix = original, constraint_matrix = b, reduced_row_echelon_form = m, solution=m[,m_cols]))
  }
  
  else return("The supplied input objects are NOT matrix objects!!")
  
    
  
}