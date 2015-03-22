# function makeCacheMatrix
# R programming assignment #2
# Coursera course in R programming
# Author:  A. J. Shaka
# Date 16 March 2015
# This function is to compute the inverse of an assumed non-singular matrix
# x and to store the result in cache, so that the compuation need not be repeated
# if the matrix has not changed.  Appears to run okay.
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# function cacheSolve
# R programming assignment #2
#Coursera course in R programming
# Author:  A. J. Shaka
# Date 19 March 2015
# This function uses the results from makeCacheMatrix to rapidly compute
# the inverse of a numerical, nonsingular (square) matrix x.  Does NOT run, however!
#
cacheSolve <- function(x = matrix(),...){
	inv <- x$getinv() #R does not accept this statement-- I can't figure out why not!
	if(!is.null(inv)) {
		message("Getting previous cached solution.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}