## This code contains two functions which work together to 
## calculate the inverse of a matrix. As this operation can 
## be time consuming, the functions evaluate whether the 
## operation has been completed before, and returns a 
## cached version if this is the case.

## The makeCacheMatrix contains a special vector that holds
## a matrix object and associated functions, such as get, set
## setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
  
        ## set matrix inverse variable to null any time a new
        ## set of data is passed into the function
        xinv <- NULL
        
        ## pass new matrix into object, clear matrix inverse
        ## variable in parent environment from pre-existing data
        set <- function(y = matrix()){
          x <<- y
          xinv <<- NULL
        }
        
        ## function to return the matrix passed into the function
        get <- function() x
        
        ## Write a supposed matrix inverse into memory. The
        ## function does not evaluate whether the input argument
        ## is truly the inverse of 'x' (it can be bogus)
        setinv <- function(somematrix) xinv <<- somematrix
        
        ## return the supposed inverse matrix stored in memory
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
