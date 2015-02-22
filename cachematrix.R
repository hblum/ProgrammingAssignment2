## This function makes a special class of matrix
## that can store it's own inverse

## This first function creates this special class
## It contains 4 items: set the value of the matrix, get the value
## Set the value of the inverse, get the value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function finds the inverse of the matrix created by the previous function
## If the inverse has already been computed, the function outputs the cached value

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
