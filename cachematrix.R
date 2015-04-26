# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

# makeCacheMatrix is a function that stores a list of functions
# set is a function that changes the matrix stored in the main function
# get is a function that returns the matrix stored in the main function
# setsolve/getsolve changes/returns the inverse stored in the main function

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# Function cacheSolve returns a matrix that is the inverse of 'x'
# cacheSolve verifies if the value m, stored previously with getsolve, exists and is not NULL. 
# If it exists in memory, it simply returns a message and the value m

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
