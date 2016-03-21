
## A function to create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## A function that returns the invers of a matrix and caches the result to m.
## If the matrix inverse has been cached the cashed is returned rather than recalculated.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting the cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
