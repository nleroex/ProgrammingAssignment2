## The following two functions create a special matrix object that can cache its inverse, 
## then calculates and returns the inverse of the matrix (if it hasn't been calculated already), or returns
## the stored inverse if it has.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL #sets the default value of inv to NULL
      set <- function(y) { #set the value of the matrix
            x <<- y #caches the matrix
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse #sets function to set inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns a list containing functions
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) { #if the inverse has already been calculated, returns it
            message("getting cached data")
            return(inv)
      }
      data <- x$get() #if not, calculates inverse
      inv <- solve(data, ...)
      x$setinverse(inv) #sets the value of the inverse matrix in the cache
      inv #Returns a matrix that is the inverse of 'x'
}
