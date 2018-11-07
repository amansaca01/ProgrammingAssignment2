# This function creates a special "matrix" object that can cache its inverse.
# It returns a list containing a functions to
# set and the value of the vector
# set and get the value of the mean

makeCacheMatrix <- function(x  = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) M <<- inv
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function.
# It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the computation.
# Otherwise, the solution is calculated and saved in the cache via the setmean function.

cacheSolve <- function(x) {
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data)
  x$setinverse(M)
  M
}
