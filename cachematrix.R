## The following 2 functions work in combination to implement cached matrix inversion.
##
## makeCacheMatrix is a 'constructor' function that creates a 'special' type of
## matrix that can cached its inverse calculation for subsequent use
##
## cacheSolve implements the actual matrix inversion on the special matric returned by
## makeCacheMatrix, taking advantage (if possible) of the existence of a previously
## cached inverse to avoid recomputing it.

## makeCacheMatrix is a 'constructor' function that creates a 'special' type of matrix.
## The matrix returned is represented as a list of accessor function that provide
## access to the underlying matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    m <- x
    
    get <- function() m
    set <- function(y){
        m <<- y
        cachedInverse <<- NULL
    } 
    
    setInverse <- function(inverse){
        cachedInverse <<- inverse
    }
    getInverse <- function() cachedInverse
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function expects a 'special' matrix constructed by the makeCacheMatrix
## function as input. It uses such a special metrix to compute the matrix's
## inverse (if not already cached) or return a cached inverse, if available.
## The argument to this function is an instance of a 'special' matrix returned by a
## call to makeCacheMatrix. The underlying matrix is expected to be invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)){
            message("getting cached matrix inverse")
            return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
