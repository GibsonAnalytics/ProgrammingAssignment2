## The makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse.  It returns a list to four functions set, get, setInverse and getInverse.
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      # Establish Functions to be used
      set <- function(y){
            # Set x in its parent environment so get has visablity to x
            # message(" In Set Function Setting x ")
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) i <<- solve
      getInverse <- function() i
      # Return list of fucnctions
      list (set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above.  If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retreive the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        # If the inverse is not null return cached copy of the inverse
        if(!is.null(i)) {
              return(i)
        }
        data <- x$get()
        # Compute the inverse.
        i <- solve(data)
        x$setInverse(i)
        i
}
