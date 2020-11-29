## This function creates a special vector which contains a list of functions 
## that sets the value vector, get the value of the vector, set the inverse of the matrix, and 
## get the inverse of the matrix.
 
##This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
            
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function determines/computes the inverse of the special matrix 
##returned by the "makeCacheMatrix" above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
