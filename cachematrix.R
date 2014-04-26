## The following 2 functions work in unison to define
## matrices that are able to cache ther inverses, once
## calculated, for later use.
## Example:
## > x <- makeCacheMatrix( matrix(c(1,2,3,4),2,2) )
## > cacheSolve(x)
## > cacheSolve(x)

## This function creates a special matrix
## that is able to cache it's inverse, once calculated
## by the function cacheSolve, defined below

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setsolve <- function(solve) inv <<- solve
		getsolve <- function() inv
		list(set = set, get = get,
			 setsolve = setsolve,
			 getsolve = getsolve)
}

## This function retrieves the inverse of a matrix
## returned by the makeCreateMatrix function. If the
## inverse has already been calculated (and cached),
## this cached value is returned without needing to
## perform the calculation. If not, it is calculated
## and cached for later use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if (!is.null(inv)) {
        		message("getting cached data")
        		return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
