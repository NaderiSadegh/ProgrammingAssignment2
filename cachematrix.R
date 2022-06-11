## Below are two functions that are used to create a special object that stores 
## a matrix and cache's its Inverse.


## makeCacheMatrix function creates a matrix and contains functions to set the 
## value of the matrix, get the value of the matrix, set the value of the 
## inverse of the matrix, and get the value of the inverse of the matrix with 
## set, get, setInv and getInv respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)
        x <<- y
    inv <<- NULL
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## The following function calculates the mean of the matrix created with the 
## above function. However, it first checks to see if the Inverse has already 
## been calculated. 

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if (!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    } 
    data <- x$get()
    Inv <- solve(data)
    x$setInv(Inv)
    Inv
}
