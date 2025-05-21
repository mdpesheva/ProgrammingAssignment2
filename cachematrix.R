### The following functions are used to create a special object that stores a matrix and caches its inverse.
### Note: For simplicity we assume the matrix supplied is invertible


## The function makeCacheMatrix creates a special “matrix”, which contains a list of functions:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function cacheSolve computes the inverse of the special “matrix” returned by makeCacheMatrix. 
# If the inverse has already been calculated, then the cachesolve should return the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#set the numeric matrix
test <- matrix(c(1,2,3,4),2,2)

#initiate first calculation of inverse matrix
test1 <- makeCacheMatrix(test)
cacheSolve(test1)

#test to check if repeat attempt for calculation of inverse gets the results from cache
cacheSolve(test1)
