##
## The following functions operate on invertible matrices,
## caching the mean for better performance.
## 


## makeCacheMatrix
##  Input: an invertible matrix 
##  Returns the Vector object containing the matrix,
##  and helper functions: get(), set(), getmean(), setmean()

makeCacheMatrix <- function(x = matrix()) { 
  	  i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  }

## cacheSolve
## Returns the inverse of the Matrix object.
## It uses the setinverse method of the 
## makeCacheMatrix vector to set the inverse in cache
## for efficiency

cacheSolve <- function(x, ...) {
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









