## R Programming
## Programming Assignment 2
## Andrew McRobie

## The two functions in this script will cache the inverse of a
##   matrix.  An example of using this function is provided in the
##   comments after the two function definitions.

## makeCacheMatrix takes a matrix as an input and produces a list
##   of four functions on the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # a function which stores a new matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # a function to return stored the matrix
  setinv <- function(solve) m <<- solve # a function to calculate the inverse of the matrix
  getinv <- function() m # a function to return the stored inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # Returns a list of four functions
}

## cacheSolve takes a matrix created in makeCacheMatrix and will
##   calculate the inverse of the matrix the first time, or
##   return the cached value of the inverse once it has been
##   calculated

cacheSolve <- function(x, ...) {
	m <- x$getinv() # tries to find a cached inverse for the special matrix x
    if(!is.null(m)) {
        message("getting cached data")
        return(m) # function ends here if cached inverse is available
    }
    # if cached inverse is not available, the inverse is computed and stored
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m) # the inverse is stored in x
    m # the inverse is returned
}

## Example: uncomment and run the following code
## b <- makeMatrix(x = matrix(c(1,2,3,4),nrow=2,ncol=2))
## class(b) # this is a list
## b$get() # this retrieves the matrix b
## cacheSolve(b) # this will compute the inverse of b
## cacheSolve(b) # this will display the stored inverse of b