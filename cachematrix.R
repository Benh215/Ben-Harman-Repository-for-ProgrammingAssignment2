
## set function in parent environment takes argument x and stores it as input via y
## set function also defines value for solve as empty (NULL) and stores 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## get function retrieves value x from parent environment
## setsolve function uses solve function to invert the input (x) matrix
## getsolve function defines m as matrix input from the parent environment, which should be null
## setsolve function uses solve function to invert the input (x) matrix
## getsolve function defines m as matrix input from the parent environment, which should be null
        
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cachesolve takes input x and attempts to retrieve an inverted array stored using the getsolve object
## if m is not null it prints the message "getting cached data" and returns the inverted array from the cache
## if m is not in the cache, input is stored as "data" object and input array is inverted
## inverted matrix is committed to cache and printed

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}