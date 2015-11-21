## makeCacheMatrix function takes as an argument a square matrix
## and returns a list of four functions, that set/get a matrix and
## set/get its inverse. This function is given as an argument to
## cacheSolve function, which returns the inverse Matrix set 
## by the first function. If the iverse Matrix is already set then
## it doesn't calculate it's value again, but it takes it from the cache.


## The following function set new matrix as well as it's inverse and get
## their data.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      ## set new matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL ## for every new matrix initially set it's inverse to NULL, so as
                         ## to calculate inverse from the beginning in cacheSolve function 
                         ## and not bring inverse stored in cache, as it correspond to old matrix
      }
      
      get <- function() x ## return matrix
      setInv <- function(inverse) inv <<- inverse ## set inverted matrix, calculated by cacheSolve function 
      getInv <- function() inv ## return inverted matrix
      list(set = set, get = get, setInv = setInv, getInv = getInv) ## returns the list of functions that
                                                                   ## will be used from cacheSolve function
}


## The following function calculate the inverse for new matrix,
## and restore inverted matrix data in a cache to avoid it's
## calculation again if it's needed.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      
      ## if matrix is the same as before, bring the it's inverse from cache
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)     
      }
      
      ## if matrix is a new one, calculate it's inverse from the beginning and store it in cache
      matrix_data <- x$get()
      inv <- solve(matrix_data, ...)
      x$setInv(inv)
      inv ## return inverse matrix
}
