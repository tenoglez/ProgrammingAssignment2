## This functions allow us to perform the inverse of a matrix object and caching its value,
## for further use, if necessary.

## This function creates a special "matrix" object that can cache its inverse. It consists of
## a list of four functions which operate with the matrix.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
