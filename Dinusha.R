## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL 
    set <- function(y) {                    
          x <<- y                             
          inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
    
  }
  
## This is used toget the cache data
  
  
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {       ##Checkingwhether invers is NULL
      message("getting cached data")
      return(inv)
      
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
}
