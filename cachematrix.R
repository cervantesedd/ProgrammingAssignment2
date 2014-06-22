## The 'makeCacheMatrix' function contains four functions that allow the user to
## set a matrix and make it invertible. The 'cacheSolve' function allows the 
## user to get the inverse of the previous matrix and cache it, as well as
## retrieve the cache. This is useful and saves time when attempting to retrieve
## the inverse of a matrix repeatedly.

## This function is used to create a matrix that is invertible.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}



## This function inverts a matrix and caches it, or retrieves the cached matrix.

cacheSolve <- function(x, ...) {
      m<-x$getmatrix()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$setmatrix(m)
      m
}
