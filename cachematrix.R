### R Programming Assignment 2
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. In this assignment I have to written a pair of functions that 
## cache the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix",  
## which is really a list containing a function to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

## This function creates a special "matrix" object that can cache its inverse.

# a matrix can be created by using matrix()
# e.g. a <- matrix(1:4, 2, 2) - numbers, nrow, ncol

makeCacheMatrix <- function(x = numeric()) {
        
        # m holds the cached value
        # if there is no cached value m is set to null
        m <- NULL
        
        # the matrix can be set either by defining x
        # or you can set it using setMatrix()
        # if you set another matrix, m will be automatically set to NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # getMatrix() will call the matrix defined by x or setMatrix()
        getMatrix <- function() x
        
        # variable where the cached inverse matrix is stored
        setSolve <- function(solve) m <<- solve
        
        # getmean() calls the variable stored in setSolve
        getSolve <- function() m
        
        # all above variables are stored in a list as part of MakeCacheMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        
        # if x$getSolve holds a value, it will be assigned to m
        m <- x$getSolve()
        
        # if m is not empty, it will retrieve the cached data and return m
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if m is empty, the inverse of the matrix held in x$getMatrix
        # will be calculated using the function solve()
        # first assign the matrix held in x$getMatrix to data
        data <- x$getMatrix()
        
        # then calculate the inverse and store it in m
        m <- solve(data, ...)
        
        # lastly assign the value stored in m to x$setSolve for future caching
        x$setSolve(m)
        
        # return m
        m
}
