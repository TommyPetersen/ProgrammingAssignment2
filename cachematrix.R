#############################################################################
## Overall description of what the functions do.
##----------------------------------------------
## makeCacheMatrix: Given a matrix, assumed to be invertible, the function
##----------------- returns a special matrix object that can cache it's
##                  inverse.
## cacheSolve:      Given a special matrix object, assumed to be invertible,
##------------      the function computes it's inverse, possibly using a
##                  cached inverse.
#############################################################################

#############################################################################
## Function name: makeCacheMatrix.
## Parameters:    A matrix object, assumed to be invertible.
## Returns:       A special matrix object that can cache it's inverse.
## Description:   The returned special matrix object is a list of
##                functions used for setting and getting the matrix
##                and it's inverse. This object state is accessed using
##                lexical scoping. For example, in the returned
##                function "setinverse", the assignment "i <<- inverse"
##                assigns the "i" which is in the body of "makeCacheMatrix"
##                and hence not a new declaration of "i" in the body
##                of "setinverse". This causes an immidiately following call
##                to the returned function "getinverse" to actually return
##                the correct assigned inverse which is that of "setinverse".
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#############################################################################
## Function name: cacheSolve.
## Parameters:    A special matrix object.
##                Some possible extra parameters for the function "solve".
## Returns:       The inverse of the matrix represented by the special
##                matrix object.
## Description:   A cached inverse is looked up in the special matrix
##                object. If the inverse was returned by the lookup,
##                then this function returns it. If the inverse was not
##                returned by the lookup, then this function computes it
##                and uses it to update the special matrix object and
##                hereafter, this function returns the inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
