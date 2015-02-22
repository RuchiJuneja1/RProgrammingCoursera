## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This assignment consist of a pair of functions that cache the inverse of a matrix.

##NOTE: assume that the matrix supplied is always invertible.


## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse 
## and returns a list of functions.
## It contains the following functions:
## setMatrix: set the value of a matrix
## getMatrix: get the value of a matrix
## setInverse: set the value of inverse of the matrix
## getInverse: get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	# initially nothing is cached, so set it to NULL	
	invMatrix <- NULL

	# store a matrix        
	setMatrix <- function(newVal) {
                x <<- newVal
		# since the matrix is assigned a new value, reset the cached value to NULL
                invMatrix <<- NULL
        }

	# returns the stored matrix
        getMatrix <- function() x

	# store/cache the inverse of given matrix
        setInverse <- function(solve) invMatrix <<- solve
	
	# get the cached value of matrix inverse
        getInverse <- function() invMatrix

	# return a list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
	# get the cached value of matrix inverse
	invMatrix <- x$getInverse()
	
	# if cached value exist, then return it. Else calculate inverse
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }

	# if cached value does not exist, calculate inverse and store it in cache
        data <- x$getMatrix()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)

	# return new calucated value
        invMatrix
}
