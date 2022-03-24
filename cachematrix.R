## Put comments here that give an overall description of what your
## functions do

# The functions objective is to write two functions that work together in the following way:
# 1) makeCacheMatrix should save and cache a data from a matrix to avoid heavy computing demands and
# 2) cacheSolve shold return the data cached. If it is already calculated, then it should retrieve the inverse 

# Computing the inverse of a square matrix can be done with the solve function in R.
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# The symbol <<- will allow us to perform the caches required in this assignment

## Write a short comment describing this function

# We use the makeVector function referenced in the asignment, replacing "numeric" by "matrix", "mean" by "inverse" and "m" by "inv".
# We create a matrix object "x" and cache its inverse "inv"

makeCacheMatrix <- function(x = matrix(1:4, nrow = 2, ncol = 2)) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

# We use the cachemean function referenced in the asignment, replacing "mean" action by "inverse" and object "m" by "inv".
# We obtain the inverse of the matrix from the cache computed by makecachematrix function above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
