## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
