## Author: Karna Adam
## Purpose: To write a pair of functions that, used together, can cache the inverse of a matrix. 
## The first function, makeCacheMatrix, takes a given matrix (an empty matrix is the default) and creates a list
## of four functions using the matrix: set, get, setinverse, and getinverse. 
## The second function, cacheSolve, returns the inverse of the matrix but takes the list created by 
## makeCacheMatrix as its inputs. If the inverse has already been calculated (and the underlying matrix has 
## not changed), then the function retrieves the inverse from the cache.
## The last (optional function) combines the two such that a matrix can be inverted in one call. 

## makeCacheMatrix takes a given matrix (an empty matrix is the default) and creates a list
## of four functions using the matrix: set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     
}



## CacheSolve, returns the inverse of the matrix but takes the list created by makeCacheMatrix 
## as its inputs. If the inverse has already been calculated (and the underlying matrix has 
## not changed), then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}

##InvertMatrix call the two functions such that a matrix can be inverted in one step. 

invertMatrix <- function(m = matrix()) {
     cacheSolve(makeCacheMatrix(m))
}


