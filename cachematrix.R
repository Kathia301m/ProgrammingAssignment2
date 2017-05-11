### Creation of a matrix (x) and computation of its inverse (inv). 
### As the computation of the matrix inverse is costy, I cache the values its values, in order to 
### avoid a recomputation if it is needed again.

## First I set and get the values of the matrix, then I set and get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the value of the matrix
    setmatrix <- function(y){
        x <<- y
        inv <<- NULL
    }
    #Get the value of the matrix
    getmatrix <- function () x
    
    #Set the inverse of the matrix
    setinverse <- function(solve) inv <<- solve
    
    #Get the inverse of the matrix
    getinverse <- function () inv
    
    
    list (setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse,getinverse=getinverse)
    
}


## Getting the inverse of the matrix with the use of the function solve()
## Computing the inverse if it hasn't been done before, otherwise, not computing it and looking up in the cache

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    # Check if the computation is in the cache
    if (! is.null(inv)){
        message ("getting cached data")
        return(inv)
    }
    
    data <- x$getmatrix()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
