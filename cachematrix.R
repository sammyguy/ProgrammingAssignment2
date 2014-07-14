## This function creates a special "matrix", which is actually a list
## containing 4 functions to: 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {x}
    setinverse <- function(inv) {m <<- inv}
    getinverse <- function() {m}
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of a INVERTIBLE (or NON-singular)
## matrix. As specified in the assignment, an invertible matrix is assumed
## so I will not include the test to check if a matrix is invertible.
## To avoid unnecessary computing, the function first checks to see if
## the inverse has already been calculated. If yes, then it retrieves 
## the inverse from the cache. Otherwise, it will calculate the inverse.
cacheSolve <- function(x) {
    ## To install the 'MASS' package which contains the ginv
    ## function, which is required to calculate generalised inverse of 
    ## a non-square matrix.
    require('MASS')
      
    ## Calls the function 'getinverse' to retrieve the inverse value 
    m <- x$getinverse()
    if(!is.null(m)) {
        ## Inverse of matrix already calculated, so just return value
        ## which is stored in 'm'
        message("Inverse already been calculated. Getting cached data..")
        return(m)
    }
    ## Inverse of matrix not calculated, proceed to calculate
    data <- x$get()
    ## Check if supplied matrix is a SQUARE matrix. If so, will
    ## use the function 'solve' for solving square matrix. Otherwise,
    ## will use the function 'ginv' to calculate a generalised inverse
    ## of the matrix (NOTE: This is not required in our assignment).
    if (isSymmetric(data)) {
        m <- solve(data)
    } else {m <- ginv(data)}
    ## After calculating the inverse, calls the 'setinverse' function 
    ## to store this value in 'm' for future retrieval
    x$setinverse(m)
    m
}