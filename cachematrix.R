## Function “makeCacheMatrix” creates a “matrix” object that can cache its inverse.
# makeCacheMatrix contains 4 functions: set, get, setmean, getmean.

makeCacheMatrix <- function(x = matrix()) {
      w <- NULL
      set <- function(m) {
            x <<- m
            w <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) w <<- solve
      getinverse <- function() w
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#Function “cacheSolve” computes the inverse of the “matrix” returned by makeCacheMatrix.
#If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
#If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, w calculates the inverse,
#and x$setmean(w) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
      w <- x$getinverse()
      if(!is.null(w)) {
            message("getting cached data")
            return(w)
      }
      data <- x$get()
      w <- solve(data, ...)
      x$setinverse(w)
      w
}
