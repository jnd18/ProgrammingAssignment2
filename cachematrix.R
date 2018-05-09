## Below are two functions that are used to create a special object 
## that stores a numeric matrix and caches its inverse.
## The first function creates the object, which is a list of four functions.
## The first two functions set and get the value of the matrix, respectively.
## The second two functions set and get the value of its inverse.
## The second function calculates the inverse of the special matrix object.
## If the inverse is already stored in the object, then it returns the
## cached result, rather than recalculating the inverse.
## If the inverse is not already cached, it calculates the inverse,
## caches the result, and returns the inverse.


## Creates a special object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse and caches it, or returns the cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
