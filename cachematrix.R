## This R file contains two functions:
##
## 1) makeCacheMatrix: This function creates 
##  a special "matrix" object that can cache 
##  its inverse.
## 2) cacheSolve: This function computes the 
##  inverse of the special "matrix" object 
##  returned by the makeCacheMatrix above. If 
##  the inverse has already been calculated 
##  (and the matrix has not changed), then the 
##  cacheSolve retrieves the cached inverse.


## The first function, makeCacheMatrix, 
## creates a special "matrix", which is 
## really a list containing a function to: 
##
## 1) set the value of the matrix,
## 2) get the value of the matrix, 
## 3) set the value of the inverse, and
## 4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function, cacheSolve, 
## calculates the inverse of the special 
## "matrix" created with the first function.
## It checks to see if the inverse has been 
## calculated. If so, it gets the inverse 
## from the cache and skips computation. 
## Otherwise, it is calculated and displayed.

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
