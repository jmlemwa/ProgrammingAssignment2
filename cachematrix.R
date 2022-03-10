## Create functions makeCacheMatrix and cacheSolve for ProgramAssignment2
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## It creates a list containing a function which
## Set the value of the matrix
## Get the value of the matrix
## Set the value of inverse of the matrix
## Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
##*****
