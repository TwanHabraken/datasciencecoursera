## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # Initially set to NULL
	# Changes when the user sets the value
  i <- NULL
  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # get function
  # Gets the matrix itself but not the inverse  
  get <- function() x
  
  # Manually set the inverse
  setInverse <- function(inverse) i <<- inverse
  
  # Get the inverse
  getInverse <- function() i
  
  # Encapsulate into a list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" by calling makeCacheMatrix unless it was already cached.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## Get the current state of the inverse and see if it has been computed yet
  i <- x$getInverse()

  ## If it has...
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If it hasn't... Get the matrix itself
  data <- x$get()
  
  ## Find the inverse
  i <- solve(data, ...)
  
  ## Cache this result in the object
  x$setInverse(i)
  
  ## Return this new result
  i
}
