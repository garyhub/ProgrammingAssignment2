## These functions perform 2 basic operations:
## 1.  Find the inverse of a matrix
## 2.  Cache the inverse, and use the cache to return the inverse on subsequent calls, when the cache is valid.
##     The cache becomes invalid when the matrix changes.
## Primary functions (described below) are:
##     makeCacheMatrix
##     cacheSolve
##     

## Create a special matrix object that can cache its inverse
## Args:
##   x:  A matrix.  Default is an empty matrix. 
## Returns:
##   A newly created matrix, initialized with internal functions:
##      set
##      get():        Returns the special matrix.
##      setinverse(): Sets the inverse of the matrix.
##      getinverse(): Returns the inverse of the matrix. If the inverse has not been generated, or if the 
##                    inverse has become invalid (due to a changed matrix), NA is returned.
##


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <<- function(newX) {   ##  Set a new matrix as the special matrix
      x <<- newX
      inverse <<-  NULL  ##  When a new matrix is set, any existing matrix is no longer valid.
                         ##  Since this is the only way to change a matrix, this is the only
                         ##    place we need to set inverse to null.  If we add other functions 
                         ##    to modify the matrix, we would need to null out the inverse there.
    }  ## end set
    
    get <- function() x  ##  Return the actual matrix within the special matrix
    
    setinverse <- function (newInverse) inverse <<- newInverse
    getinverse <- function () inverse
    
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


##    Return the inverse of a special matrix that has been created with makeCacheMatrix.
##    If an inverse has been cached, and the cache is still valid, the cached inverse is returned.
##    If the cache is invalid, or the cache is empty, the inverse is created and set.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) { ## cached inverse is valid, so return it
##    message ("gettting cached matrix")
    return (inverse)   ## return the cached inverse
  }
  
## Cached data is not valid.  Determine it and set it in the special matrix
  rawMatrix <- x$get()
##  message ("calculating inverse for: ", rawMatrix)
  inverse <- solve (rawMatrix, ...)
  x$setinverse(inverse)
  inverse    ## Return the newly computed inverse
}
