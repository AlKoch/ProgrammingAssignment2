##
##
## makeCacheMatrix - This creates a list object containing four functions 
## for use by cacheSolve() in the efficient computation of the inverse of a 
## matrix.  The interesting part of makeCacheMatrix() is that when it's 1st 
## created it retains the value of the matrix supplied as an input parameter.
## Then it caches within itself the value of the matrix inverse (once it has 
## been created).
##
## cacheSolve - This depends upon having a makeCacheMatrix list object 
## supplied to it.  Upon the 1st call with a particular instance of 
## makeCacheMatrix it computes the inverse of makeCacheMatrix's "internal" 
## matrix and uses makeCacheMatrix's functionality to cache that inverse within 
## the makeCacheMatrix object.  Upon subsequent calls *WITH THE SAME INSTANCE* 
## of the makeCacheMatrix object it just fetches the cached inverse matrix.  
## Note that this only happens if we call cacheSolve with the same instance of 
## makeCacheMatrix.  For example, note what happens if we do this:
## > cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
## Computing Inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
## Computing Inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## In this scenario it appears that we are calling cacheSolve() with "the same 
## value" but we don't fetch the cached value because the 2nd call to 
## cacheSolve() creates a new instance of a makeCacheMatrix list object.
## 
## 
## Test case:
## > mtrxCache<-makeCacheMatrix(matrix(1:4,2,2))
## > cacheSolve(mtrxCache)
## Computing Inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mtrxCache)
## Fetching cached inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > matrix(1:4,2,2)%*%cacheSolve(mtrxCache)
## Fetching cached Inverse
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
makeCacheMatrix <- function(mtrx = matrix()) {
    inv <- NULL
    set <- function(y) {
        mtrx <<- y
        inv <<- NULL
    }
    get <- function() mtrx   # this is "where" the matrix argument is "stored"
    # This fcn doesn't compute the inverse but only stores the inverse 
    # passed in.
    setinverse <- function(inverse) {
        message("Computing Inverse")
        inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Fetching cached Inverse")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setinverse(inv)
    inv
}
