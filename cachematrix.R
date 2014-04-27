## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(ma = matrix())
{

  inv_ma   <- NULL
  ind      <<- 0                            # use ind <<- indicate whether the matrix changed  
  setma    <- function(y)                   # set matrix value    
  {
    verif(y)                                # verifies that the matrix changed to update the value of ind
    ma     <<- y
    if (ind == 0) inv_ma <<- NULL           # 
  }
  getma    <- function() ma                 # get matrix value
  setinv   <- function(inv)                   
  {                                         # set inverse matrix
    inv_ma <<- inv                           
    ind    <<- 1                       
  }  
  getinv   <- function() inv_ma             # get inverse matrix
  verif    <- function(y)                     
  {
    nr  = nrow(y)                           
    nc  = ncol(y)                           
    ind <<- 1                               # matrix indicator
                                            # ind == 1 -> matrix is not changed
                                            # ind == 0 -> matrix changed 
    if ((nr == nrow(ma)) && (nc == ncol(ma)))   # verifying  size matrix
    {
      message("Tamanho igual -> verificando elementos")          
      for (i in 1:nr) {                     # compare elements of matrix   
        for (j in 1:nc) {
          if (y[i,j] != ma[i,j]) 
          {
            ind <<- 0

          }
        }
      }
      if(ind == 0 ) message("Elementos diferentes Matrix mudou")
      else          message("Elementos iguais")
    }
    else
    {
      message("Tamanhos Diferentes -> A matrix mudou")         
      ind <<- 0
    }
    ind
  } 
  
  list( setma  = setma, 
        getma  = getma, 
        setinv = setinv, 
        getinv = getinv,
        verif  = verif)
}



## Write a short comment describing this function


cacheSolve <- function(ma, ...) 
{
  inv_ma <- ma$getinv                        # get inverse matrix
  if (!is.null(inv_ma) && (ind == 1))        # inverse matrix is NULL? and  Matrix changed? 
  {
    message("getting cached inverse matrix")
    return(inv_ma)                           # return value cached
  }                                                
  message("seeking - inverse matrix") 
  data   <- ma$getma()                        
  inv_ma <- solve(data)                      #  
  ma$setinv(inv_ma)                          # save inverse matrtix    
  inv_ma                                     # return inverse matrix   
}
