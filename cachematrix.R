## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cach
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
# create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
# get the value of the matrix
  get <- function() x
# invert the matrix and store
  setinverse<- function(inverse) inv_x <<-inverse
# get the inverted matrix
  getinverse <- function() inv_x
# return the created functions to the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
## attempt to get the inverse of the matrix stored in cache
  inv_x <- x$getinverse()
# return inverted matrix from cache if it exists  # else create the matrix in working environment
  if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
# set inverted matrix
        x$setinverse(inv_x)
        return(inv_x)
    }
}
