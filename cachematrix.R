## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
c=rbind(c(1, -1/4), c(-1/4, 1))
c
solve(c)
cacheSolve(c)
x=makeCacheMatrix(c)
cacheSolve(x)
