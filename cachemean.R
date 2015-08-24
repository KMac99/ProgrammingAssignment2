makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

##  testing
# to run these functions, try the following code in command line:
# > v1 = c(1:100)
# > mv1 = makeVector(v1)
# > mv1                   mv1 here is a list: that is an environment containing 
# the original vector v1 and its mean.
#  a second caching:
# > v2 = c(1:200)
# > mv2 = makeVector(v2) 
# > mv2
#You can test how that works by running this code (after running that above):
#        
# >  cachemean(mv1)
# >  cachemean(mv2)
# >  cachemean(mv1)
# >  cachemean(mv2)