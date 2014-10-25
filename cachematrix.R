## This code contains two functions which work together to 
## calculate the inverse of a matrix. As this operation can 
## be time consuming, the functions evaluate whether the 
## operation has been completed before, and returns a 
## cached version if this is the case.

## The makeCacheMatrix contains a special vector that holds
## a matrix object and associated functions, such as get, set
## setinverse, and getinverse

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

## The cacheSolve function returns the inverse of an invertable
## square matrix. If this solution already exists in memory, it 
## is pulled from the cache; otherwise, it is calculated and 
## passed into makeCacheMatrix

cacheSolve <- function(x, ...) {
  
        xinv <- x$getinverse()
        
        ## CASE 1: matrix inverse exists in cache
        
        if(!is.null(xinv)) {
          message("getting cached data")
          return(xinv)
        }
        
        ## CASE 2: matrix inverse must be calculated
        ## and stored in memory
        
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        
        return(xinv)
}
