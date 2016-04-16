# (Note for assessors; if you are interested, there is a plot of how much faster
# the cache is at cacheVsUncachedPlot.png, automated tests in 
# cachematrix.tests.R, and the code to generate the plot at cachematrix.plot.R.
# Thanks! -- Steve)

#' Create a wrapper to store a matrix, and, if has been calculated, store its 
#' inverse. Use to optimise code that is slow due to the \code{solve} method. 
#' Current implementation is ~15x faster for 3x3 matrices, improving to ~250x
#' faster for 50x50 matrices.
#' 
#' @param x a matrix
#' @return a list of functions for storing a matrix and its inverse.
#' @examples
#' 
#' cacheMatrix(x=matrix(c(1, 0, 0, 
#'                        0, 1, 0, 
#'                        0, 0, 1), nrow = 3, ncol = 3))
#'                        
makeCacheMatrix <- function(x = matrix()) {
    # create now so that the `set` and `setinverse` functions have variables to
    # overwrite.
    stored.inverse <- NULL 
    stored.matrix <- NULL
    
    # a guard function -- we want to make sure we only store matrices!
    ensure.is.matrix <- function(x) {
        if (!is.matrix(x)) {
            stop("not a matrix")
        }
    }
   
    # return four functions we can use to get and set data
    result <- list(
        
        set = function(new.matrix) {
            ensure.is.matrix(new.matrix)
            stored.matrix <<- new.matrix
            stored.inverse <<- NULL # cache is invalid
        },
        
        get = function() {
            stored.matrix
        },
        
        setinverse = function(new.inverse) { 
            ensure.is.matrix(new.inverse)
            stored.inverse <<- new.inverse
        },
        
        getinverse = function() {
            stored.inverse 
        }
    )
    
    # call now to store the formal parameter, x.
    result$set(x)
    
    result
}

#' This takes a 'cacheMatrix' value (as created by 'makeCacheMatrix') and 
#' returns the inverse of the matrix held inside. If it has already been
#' calculated, the cached value is returned. If not, it is set as a side effect.
#' 
#' @param x a 'cacheMatrix' value
#' @return the inverse of the matrix held in x.
#' @examples
#' cacheSolve(x=myCacheMatrix)
cacheSolve <- function(x, ...) {
    # do we already know the answer?
    inv <- x$getinverse()
    if (is.null(inv)) {
        # the inverse isn't set -- solve it and store it.
        inv <- solve(x$get())
        x$setinverse(inv)
    }
    
    # now we've definitely got the inverse, so return the cached version
    inv
}
