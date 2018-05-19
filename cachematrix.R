## Caching the inverse of a matrix
## The two functions below work together to determine the inverse of a square matrix and cache it
## in memory so that the inverse can be retrieved, thus avoiding the need to recalculate the inverse. 
## These functions demonstrate the concept of lexical scoping.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix as not changed), then 
## cachSolve retrieves the inverse from the cache.
cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
