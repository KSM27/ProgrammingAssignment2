## Below function makeCacheMatrix does the following:
## store a martix and a cached value of the inverse of the matrix and has the following functions:
## setMatrix      set the value of a matrix
## getMatrix      get the value of a matrix
## cacheInverse   get the cahced value (inverse of the matrix)
## getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL                                    # assigning Null to inverse value
 set <- function(y) {                           # defining set function
        x <<- y                                 # matrix value in parent environment
        inv <<- NULL                            # resetting inverse to Null for new matrix
    }
    get <- function() x                         # returning the value of matrix
    setinverse <- function(inverse) inv <<- inverse     # assiging inverse value in parent environment
    getinverse <- function() inv                        # gets the value of inverse where called
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## makeCacheMatrix
## The below function calculates the inverse of a special matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        
    inv <- x$getinverse()              # get the cached value
    if(!is.null(inv)) {                # if a cached value exists, return it
        message("getting cached data.")
        return(inv)
    }    
 # If not then get the matrix, caclulate the inverse and store it in cache
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv                            # Return the inverse
}
