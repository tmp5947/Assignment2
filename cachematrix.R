## makeCacheMatrix and cacheSolve work together to 
## calculate the inverse of a matrix and use the cached
## inverse to avoid recalculating it.


## makeCacheMatrix: Calculates the inverse of a matrix and stores the 
## inverse in 'i' for later use

makeCacheMatrix <- function(x = matrix()) {
  # 'i' stores the matrix inversion.  Initialize it to NULL.
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # return matrix 'x'
  get <- function() {
    x
  }
  
  # store the matrix inverse in 'i'
  setInverse <- function(solve) {
    i <<- solve
  }
  
  # return the cached inverse
  getInverse <- function() {
    return(i)
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Returns the inverse of matrix 'x'.   
##
## Assumes that the matrix is square and invertible.
##
## Will return the stored inversion rather than
## calculating it if the cached inversion is available.
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse of the matrix")
    return(i)  # return the cached version
  }
  
  # If cached inversion was not available, calculate it with 'solve'
  # and store it using setInverse()
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  return(i)  # return the calculated inversion
}

