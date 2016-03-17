## The first function creates a matrix object that creates and caches its own inverse. The second 
## checks to see if the matrix has already had its inverse calculated. If so, it retrieves the inverse from the cache,
## if not it calculates the inverse. 

## This creates a matrix object that can create and cache its own inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns a matrix that is the inverse of X. It first checks to see if the inverse already exists.
## If so, it returns it. If not, it calculate the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}


