## File contains functions responsible for computing matrix inverse and caching the inverse
## for further calls.
## Introducing a special matrix created by "makeCacheMatrix" by passing to it an object of 
## class() "matrix". The inverse of special matrix can be computed by "cacheSolve" and if the
## same special matrix got passed to "cacheSolve" it returns the cached inverse instead of 
## recomputing the inverse.

## Function create a special "matrix" which is really a list containing, functions
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Function calculates the inverse of the special "matrix" created from the function
## makeCacheMatrix. First it checks if the inverse already calculated, returns it from 
## cache. If not cached it make the computation of inverse and stores it in the cache.
## Note: The function notifies the user whether it retrieved the inverse from the cache
## or its is the first time to store it in the caches

cacheSolve <- function(x, ...){
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("Getting cached matrix inverse")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            message("Storing the inverse of matrix in cache")
            m
}