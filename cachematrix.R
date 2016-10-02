## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x_inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) 
    x_inv <<- inv
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInv()
  if (!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  matrix <- x$get()
  x_inv <- solve(matrix, ...)
  x$setInv(x_inv)
  x_inv
}
