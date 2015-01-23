## makeCacheMatrix and cacheSolve work together to more efficiently calculate
## the inverse of any inversible matrix by creating a special object to store 
## the matrix and cache its mean

## makeCacheMatrix creates a list containing function to set and get the value
## of the matrix and to set and get the value of its inverse, using the solve
## function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve calculates the inverse of special "matrix" created above
## but first checks the cache to see if it's already been calculated.
## If not, it calculates the inverse and stores it to the cache via setsolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}
