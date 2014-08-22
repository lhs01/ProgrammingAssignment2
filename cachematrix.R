## Functions to calculate the inverse of a matrix using caching

## makeCacheMatrix creates a list containing 4 functions
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the inverse of the matrix
## getinverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachSolve calculates the inverse of a matrix (created with the function makeCacheMatrix)
## uses the cached version of the inverse if it has already been calculated, otherwise calculates 
## and caches the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
}
