## The functions create a special matrix that can keep 
## cache of the matrices data and its inverse

## The first function, makeCacheMatrix creates a special matrix 
## that is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function(x)
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list( set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The second function cacheSolve computes the inverse of
## the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
