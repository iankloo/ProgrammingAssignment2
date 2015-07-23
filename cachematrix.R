## this code uses 2 functions to calculate, cache and return the inverse of a matrix.
## specifically, the procedure caches the inverse on the first run, then reports this
## result from memory on subsequent runs.  this is the general case of the concept of 
## caching to avoid rerunning time expensive code.

## function makes list of functions to set the value of the matrix, get the value of the matrix
## set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function calculates the inverse of the matrix.  if the inverse was already calculated and cached,
## the function prints the cached value and a message noting that it was already cached.
## if the inverse is not cached, the function calculates and returns it.

cacheSolve <- function(x, ...) {
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

##example test code
#z <- matrix(data=c(1,0,5,2,1,6,3,4,0), nrow=3)
#a <- makeCacheMatrix(z)
#cacheSolve(a)


