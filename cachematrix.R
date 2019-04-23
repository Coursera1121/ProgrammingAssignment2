## makeCacheMatrix acts a class where the user can create a matrix and interact
## with the various functions. cacheSolve returns the inverse of a matrix

## This function creates a matrix and allows the user to fill in the values
## as well as set and get an inverse and get the data of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inve) inv <<- inve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a matrix by first checking if it
## already exists and if not then computes it using the solve function

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
