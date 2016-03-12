##makeCacheMatrix & cacheSolve work together to cache the inverse of a matrix
##so R doesn't have to solve for the inverse every time. Saves time!

## makeCacheMatrix takes in a matrix, set up the list of functions.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) n <<- solve
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve caches the inverse of the matrix. Once this is run I can type
##z$getinverse() where z<-makeCacheMatric(some_matrix) and also get the inverse

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}    ## Return a matrix that is the inverse of 'x'

