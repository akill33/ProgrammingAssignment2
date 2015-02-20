## This set of functions is to cache the inverse of a matrix and return the cached inverse
## when called. If there is no inverse in cache, it will solve for the inverse

## The function bellow makes a list. It is actually a list with 4 functions - 
## 1. set the matrix
## 2. Get the Matrix
## 3. Set the Inverse 
## 4. Get the Inverse. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The following function solves for the Inverse of the Matrix after checking the cahce. 
## If the inverse is stored in the cache already, it returns that value, else it calculates
## the inverse, stores it in the cache and returns the inverse for us. Magic! 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
