#My name is Abel and this is my week 3 programming assignment for the course R programming
#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
#Creating a matrixmakeCacheMatrix function
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
      m
  }
  setInverse <- function(inverse) {
      i <<- inverse
  }
  getInverse <- function() {
      i
  }
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
  }
      
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
  }