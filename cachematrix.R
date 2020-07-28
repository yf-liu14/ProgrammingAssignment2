## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function will return a list containing four functions:
## set is used to set a new matrix
## get is used to get the value of a matrix
## setinv is used to cache the inverse of the matrix
## getinv is used to get the inverse of the matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function calculate the inverse of a matrix. First, it will check 
## whether the inverse has already been calculated. If it has been calculated
## a message - "getting cached data" will appear, and the cached inverse matrix
## will be printed. If the inverse has not been calculated, it will calculate
## the inverse of the matrix using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}

