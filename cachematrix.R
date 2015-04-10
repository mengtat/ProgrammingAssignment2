## Assignment: Caching the inverse of a matrix
##
## Inversing a matrix can be computationally expensive. In
## this assignment, I will write a function to cache the result 
## of an inversed matrix.
##

## makeCacheMatrix will can cache an inverse of a given matrix. 
## If the matrix is changed, the cache will be cleared.
makeCacheMatrix <- function(x = matrix()) {  
    inverse <- NULL
    
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(i) inverse <<- i
    
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix . 
## If the inverse is already cached by makeCacheMatrix, 
## it will use the cache. If not, it will calculate the 
## inverse and cache it using makeCacheMatrix.
##
## x is an instance of makeCacheMatrix
##
cacheSolve <- function(x, ...) {
  
  ## try to get it from the cache
  i <- x$getinverse()
  
  ## if it is there, just returns it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## get thematrix
  data <- x$get()
  
  ## inverse it
  i <- solve (data, ...)
  
  ## update the cache
  x$setinverse(i)
  
  ## return the matrix that is the inverse of 'x'
  i
}

## x <- matrix(c(1:4), nrow = 2, ncol = 2)
## y <- makeCacheMatrix(x)
## cacheSolve(y)