## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y = matrix()){
          x <<- y
          xinv <<- NULL
        }
        get <- function() x
        setinv <- function(somematrix) xinv <<- somematrix
        getinverse <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        xinv <- x$getinverse()
        if(!is.null(xinv)) {
          message("getting cached data")
          return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
