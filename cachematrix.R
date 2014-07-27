## Pair of functions to calculate and cache the inverse of an invertible matrix

## makeCacheMatrix function creates a four-item list to set value of matrix,
## get value of matrix, set inverse of matrix, get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialise and define list items for matrix and its cached inverse
        ## to be used by cacheSolve
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function checks whether inverse of matrix has already been computed
## If inverse is available it returns the cached value
## If inverse has not yet been computed it is calculated using the solve function
## cacheSolve assumes that the matrix supplied is invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Test whether inverse is available - is getinv of this matrix non-null?
        ## If inverse is available display message and return the inverse
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
        ## If inverse is not available then get the matrix to be used and calculate
        ## the inverse using set. Assign this value to setinv so that it is cached
        ## Return the newly calculated inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
