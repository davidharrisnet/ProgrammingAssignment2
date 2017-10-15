##
## The following functions operate on invertible matrices,
## caching the mean for better performance.
## 


## makeCacheMatrix
##  Input: an invertible matrix 
##  Returns the Vector object containing the matrix,
##  and helper functions: get(), set(), getmean(), setmean()

makeCacheMatrix <- function(x = matrix()) {
   aVector <- makeVector(x)  
   aVector  

  }

## cacheSolve
## Returns the inverse of the Matrix object returned from 
##  makeCacheMatrix(), and caches its mean

cacheSolve <- function(x, ...) {
      inversM <- solve(x$get(), ...)
	cachemean(x)
      inversM
}



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

