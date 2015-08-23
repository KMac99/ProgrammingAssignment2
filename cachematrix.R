## Programming Assignment 2, R programming Coursera
# this program creates a pair of functions to cache the inverse
# of a matrix.  

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <-matrix(data = NA, nrow = 1, ncol = 1)  # create a null inverse matrix
        set <- function(y) {
                x <<- y
                inv <<-matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                             dimnames = NULL)
        }
        
        get <- function() x
        setinv <- function(solve) y <<- solve(x)  #solve(x)  # this function computes the inverse of a matrix
        getinv <- function() y
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {      ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv[1,1])) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}