## Put comments here that give an overall description of what your
## functions do
## cachematrix.R solves for the inverse of a square matrix, and keeps said inverse matrix in a cache using two functions.
## Second function tries to retrieve the cache, the value of the inverse matrix, if possible. 
## First function acts as the cache and stores said inverse matrix

## Write a short comment describing this function
## Creates a function that allows to set and get the variables as well as the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                               ## m as the variable for the inverse of matrix x
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x                     ## get as a function that takes in values of x matrix
    setInv <- function(inv) m <<- inv       ## second part of the code to transfer the value of m from cacheSolve to makeCacheMatrix
    getInv <- function() m                  ## function that cacheSolve calls to get the value of m, aka the cached data
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
## Solves the inverse of a matrix, and returns the value (m) to makeCacheMatrix for caching

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()                        ## Gets the value of the inverse matrix from makeCacheMatrix (if it exists)
    if(!is.null(m)){                       ## If m is not NULL, returns m, aka inverse of the matrix
        message("getting cached data")
        return(m)
    }
    data <- x$get()                        ## Gets the value of the input matrix
    m <- solve(data, ...)                  ## Solves for the inverse of the input matrix
    x$setInv(m)                            ## first part of the code to set the value of m (inverse matrix) to cache (makeCacheMatrix)
    m
    
    
}
