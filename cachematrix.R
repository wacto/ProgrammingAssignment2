# This function creates a special "matrix" object that can 
# cache its inverse, which is really a list containing a function to
#   1. set the matrix
#   2. get the matrix
#   3. set the inverse of the matrix
#   4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. 
# If the inverse has already been calculated,  then the cacheSolve
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setinverse(s)
  s
}

# Example of use
#  c<-rbind(c(1, -1/4), c(-1/4, 1))
#  x<-makeCacheMatrix(c)
#  cacheSolve(x)
