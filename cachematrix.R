## Programming Assignment 2, R programming Coursera
# this program creates a pair of functions to cache the inverse
# of a matrix.  

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <-matrix(data = NA, nrow = 2, ncol = 2)  # create a null inverse matrix
        set <- function(y) {
                x <<- y
                inv <<-matrix(data = NA, nrow = 2, ncol = 2) 
        }
        
        get <- function() x   # get and return the cached matrix x
        setinv <- function(newinv) inv <<- newinv  # assign some matrix y to be the inverse
        getinv <- function() inv  # get and return the cached matrix inverse of x: "inv"
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
        inv
        if(is.na(inv[1,1])==FALSE) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        message("solving inverse and caching")
        x$setinv(inv)
        #inv
}

##  test code
# > a<- matrix(1:4, 2,2)
# > b <- makeCacheMatrix(a)
# > cacheSolve(b)