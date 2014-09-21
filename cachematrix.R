## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ##stores the functions necessarry to input the inverse
      ## x and vec outside this environement
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(solve) inv <<- solve
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## cacheSolve resolves the inverse of the matrix if already calculated
        ## if it is not calculated it does it
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInv(inv)
      inv
}

