## The below function makeCacheMatrix() does the following:
## (1) sets the value of a matrix
## (2) gets the value of that matrix
## (3) sets the inverse of that matrix
## (4) gets the inverse of that matrix
## The matrix supplied *must* be invertible for this function to work.
##
## Example usage for setting up the intial matrix:
##  my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##
## Subsequent elements can be retrieved in the console by, e.g.:
##  my_matrix$get()
##  my_matrix$getinv()
## Note that inverse calls will return NULL if the cacheSolve() function has not
## been run yet after defining my_matrix.
##
## To set a new matrix value, e.g.:
##  my_matrix$set(matrix(5:8, 2, 2))
## Setting a new matrix overwrites inverse values that may have previously been
## cached by the below function cacheSolve() to NULL.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The below function cacheSolve() will calculate, return, and cache the inverse
## of the matrix created by makeCacheMatrix() - identified by 'x'.
## If the function has previously been run on the matrix defined by
## makeCacheMatrix() and that matrix has not been changed, then the calculation
## step ["solve()"] is skipped and the cached value is returned instead. This
## situation is identified when the message "getting cached data" is printed to 
## the console.
##
## Example usage:
##  cacheSolve(my_matrix)
## makeCacheMatrix() *must* have been run first to define my_matrix.

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


## Further examples of this script in use:
# > source("cachematrix.R")
# > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# > my_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > my_matrix$getinv()
# NULL
# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(my_matrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > my_matrix$set(matrix(5:8, 2, 2))
# > my_matrix$getinv()
# NULL
# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > 
