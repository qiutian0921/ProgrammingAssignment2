## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
    get <- function() x
    setinvs <- function(inversematrix) invs <<- inversematrix
    getinvs <- function() invs
    list(set=set, get=get, setinvs=setinvs, getinvs=getinvs)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invs <- x$getinvs()
    if (!is.null(invs)){
        message("getting cached data")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinvs(invs)
    invs
}
