## Getting the inverse of a matrix can be time and memory consuming. The following 
## code allows the user to search in the environment if the inverse of a certain matrix
## has been computed. In case of finding the result stored, the result is re-used 
## avoiding to calculate the value again. Otherwise, the inverse is computed.


## The code is divided in two functions:
##      1) makeCacheMatrix: creates a "vector" (list of functions) that allows the user
## to set or get matrix values and set or get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y             # allows the user to use x in get function
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse ## allows user to use inv in getinverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This function gets a list of matrix information (input)  and allows the user to check if 
## the inverse of the matrix has been computed. If a previous value is found, it is re-used.
## Otherwise the inverse is computed and stored in the input list.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv ## Returns a matrix that is the inverse of 'x'
}

## Testing the code

r_dat = rnorm(250000)
mat_r = matrix(r_dat, nrow=500, ncol=500)
mat_r_inv <- makeCacheMatrix(mat_r)
cacheSolve(mat_r_inv)
cacheSolve(mat_r_inv)
