## This pair of functions allows you to store the inverse of a matrix so
## it can be called in the future without having to be recalculated


## makeCacheMatrix initializes the function argument as x,
##  sets the value of m to NULL and defines four other functions
## the four other functions are stored in a named list so they can easily be retrieved


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() {inv}
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve checks to see if there is a stored value of m, the inverse matrix
## if there is it returns it, if it is NULL it calculates it, stores it, and then prints it

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
