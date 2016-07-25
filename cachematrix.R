## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list with four items
## 1) set: a function that sets the value of the initial 
## matrix in cache
## 2) get: a function that retrieves the stored matrix
## 3) setinverse: a function that sets the value of the inverse
## 4) getinverse: a function that retrieves the stored inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve is a function that is called to retrieve the inverse
## of a matrix and store it in cache.  If the input matrix is not 
## changed, subsequent calls to cacheSolve will return the stored
## inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
