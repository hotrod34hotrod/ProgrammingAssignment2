## These function store a matrix and cache its inverse. This inverse can
##Then be used in the future if the matrix has not changed in order to avoid 
##recalcualting the inverse, in order to save computing time.

## This function creates a special matrix object that can store its own
## inverse. It returns a list of functions that let you set the matrix,
## get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(new_mat) {
    mat <<- new_mat
    inv <<- NULL     
  }
  get <- function() mat
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function computes the inverse of the special matrix created above.
## If the inverse has already been calculated and stored, it will return
## the cached value. If not, it calculates the inverse, stores it, and
## then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Returns cached inverse if present
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  #compute inverse if not cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  return(inv)
}