## R Programming - Programming Assignment 2 - Lexical Scoping

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y # parent variable x assigned value y
                inverse <<- NULL # parent variable "inverse" matrix set to NULL
        }
        get <- function() {
			x #return parent variable x
		}
        setInv <- function(invMatrix) {
			inverse <<- invMatrix #set parent variable "inverse" matrix 
		}	
        getInv <- function() {
			inverse # return parent variable "inverse" matrix
		}
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
		# get the cached inverse matrix
        invmtrx <- x$getInv()
		
		# check if cached inverse is available. If so, return the cached inverse matrix
        if(!is.null(invmtrx)) {
                message("getting cached data")
                return(invmtrx)
        }
		
		# else cache the inverse of the matrix
        data <- x$get()
		
		# solve(X) computes inverse for a square invertible Matrix X
        invmtrx <- solve(data)
        
		x$setInv(invmtrx)
        invmtrx
}