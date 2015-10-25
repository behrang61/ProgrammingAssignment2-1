# Assignment No. 2 (Cache Matrix)
# The following website is visited to develop the program 
# Got some help to figure out how it works.
# Link: http://xmuxiaomo.github.io/

makeCacheMatrix <- function(x = matrix()) {
        ttnv <- NULL
        set <- function(y) {
                x <<- y
                ttnv <<- NULL
        }
        get <- function() x
        setttnverse <- function(ttnverse) ttnv <<- ttnverse
        getttnverse <- function() ttnv
        list(set = set,
             get = get,
             setttnverse = setttnverse,
             getttnverse = getttnverse)
}

cacheSolve <- function(x, ...) {
        ttnv <- x$getttnverse()
        if (!is.null(ttnv)) {
                message("Cached Data")
                return(ttnv)
        }
        ut <- x$get()
        ttnv <- solve(ut, ...)
        x$setttnverse(ttnv)
        ttnv
}
