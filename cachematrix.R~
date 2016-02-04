## The following two functions are used to
## find the inverse of a matrix and store it in a 
## cache. If the matrix is not changing, the inverse
## can be retrieved from the cache instead
## of computing it all over again, thus saving time.
## Here's a great link which explains how these
## functions work:
## https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/

## The following function returns a list of functions set, get,
## setsolve and getsolve, to set the matrix, get the matrix, set its
## inverse and get its inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) m <<- inv
        getsolve <- function() m
        list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)

}


## The following function checks if the inverse is stored in the cache.
## If yes, then it takes the inverse from the cache instead of computing it.
## If not, it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
