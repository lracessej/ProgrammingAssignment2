## What cacheMatrix will take a matrix, and store it for later use.
## When the inverse is calculated, cacheMatrix will store the inverse for later.
## If the inverse is calculated again, cacheMatrix will instead return the previously
## calculated inverse, unless the matrix has been changed.

## makeCacheMatrix will store a matrix, and its inverse the first time it is calculated
## using cacheSolve.
##
## x is a matrix which will later be used to find its inverse.

makeCacheMatrix <- function(x = matrix()){
  z <- NULL                             # We don't know the inverse, so initialize the value as null
                                        # This also helps to overwrite any previously cached values.
  set <- function(y) {                  # This allows anyone to reinitialize the values in this function.
    x <<- y
    z <<- NULL
  }
  get <- function() x                   # Returns the original matrix
  setInverse <- function(inv) z <<- inv # Sets the inverse of the matrix.
  getInverse <- function() z            # Returns the inverse of the matrix.
  list(set = set, get = get,            # Returns the list of functions for cached Matrices
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will first check to see if the inverse of the given
## matrix has been solved or not. If so, it will return the inverse
## already stored by makeCacheMatrix. If not, it will calculate the inverse,
## store the result using one of the makeCacheMatrix functions, then 
## return the inverse.
##
## x is a list of functions set by makeCacheMatrix.
## It stores functions used to access the cached matrices,
## and overwrite them.

cacheSolve <- function(x, ...) {
  z <- x$getInverse()      # Checks for an inverse.
  if (!is.null(z)) {     # If  the inverse has previously been calculated,
    message("getting cached data")
    return(z)            # Return the previously calculated inverse.
  }
  data <- x$get()        # Otherwise, get the matrix.
  z <- solve(data)       # Caculate the inverse.
  x$setInverse(z)        # Store the inverse for later.
  z                 ## Return a matrix that is the inverse of 'x'
}
