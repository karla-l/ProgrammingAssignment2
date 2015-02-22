## The following functions compute the inverse of a square matrix
## and cache the resultant using two functions: 
##[1] makeCacheMatrix to create matrix object and store inverse
##[2] cacheSolve to compute matrix inverse and return cached inverse if already calculated

## The function makeCacheMAtrix creates a matrix object and
## uses getinverse and setinverse to store and return a cached version
## the matrix inverse. 
## If the original matrix is changed, the cached
## inverse is invalid and passed to cacheSolve to be recalculated 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##inverse is initiated as an uncalculated value
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The function cacheSolve calculates and returns 
##the inverse of the matrix passed in makeCacheMatrix. 
## If the inverse has already been calculated 
## the function returns message "Inverse already calculated" and
## output inverse stored in makeCacheMatrix.
## If the inverse has not yet been calculated or a new matrix input
## the function computes the inverse of the matrix using the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Inverse already calculated")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...) ##Calculates matrix inverse
  x$setinverse(inverse)
  inverse
}
