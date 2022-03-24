# Set up example matrix
mat1 = c(1,2,3,4)
mat1 = matrix(mat1,nrow=2,ncol=2,byrow=TRUE)


## Fxn 1: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Setting up inverse matrix as null
  m <- NULL
  
  set <- function(matrix) { # Fxn 1 for setting value of matrix ("set")
    x <<- matrix # Set x as y within makeVector fxn env
    m <<- NULL # Overwrite m in makeVector fxn env with NULL
  } # End of set fxn
  
  get <- function() { # Fxn 2 for getting value of vector ("get")
    x } # Print matrix
  
  setInv <- function(solve) { # Fxn 3 for setting value of vector
    m <<- solve } 
  
  getInv <- function() { # Fxn 4 for getting value of mean
    m } # Print inverse matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) # Creating list of output of 4 fxns
}


## Fxn 2: computes inverse of 


cacheSolve <- function(x, ...) { # ... means we can add extra arguments
  
  m <- x$getInv() # Pull mean from above function
  
  if(!is.null(m)) { #If m exists
    message("getting cached data")
    return(m) # spit out m
  }
    
  data <- x$get() # Get X matrix
  m <- solve(data) # Calc inverse with matrix mult 
  
  x$setInv(m) # Set the object to be inverse
  
  m # Return matrix
}  



# cacheSolve <- function(x, ...) {
#   m <- x$getsolve() # Pull mean from above function
#   if(is.null(m)) { # If m does not exist
#     message("calculating inverse")
#     solve(m) # Att: calc inv here
#     return(m)
#   }
#   if(!is.null(m)) { # If m exists
#     message("getting cached data")
#     return(m) # spit out m
#   }
#         ## Return a matrix that is the inverse of 'x'
# }
