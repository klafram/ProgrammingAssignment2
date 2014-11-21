## The below functions work together to take a matrix and calculate the inverse.
## If the inverse has been calculated, the stored value is "inv".
## If it has not been calculated, inv = NULL and cacheSolve will calculate it and cache.

## This function takes a matrix and generates a list of functions to 
## get and set the matrix data and cached inverse.

makeCacheMatrix <- function(x = matrix()) { ## input is matrix
        inv <- NULL  
        set <- function(new) { ## can use this function to set x to a new matrix
                x <<- new      ## also restores inv to NULL each time x is reset
                inv <<- NULL
        }
        get <- function() {x} ## use this function to retrieve matrix
        setinv <- function(inverse) {inv <<- inverse} ## use to store inv
        getinv <- function () {inv} ## access current inv value
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}

## cacheSolve accesses the functions in the list generated my makeCacheMatrix.
## If inv has already been calculated it retrieves it from object made by makeCacheMatrix. 
## If it hasn't, it calculates, caches/stores in it the object created by makeCacheMatrix.
## Also, prints inv.

cacheSolve <- function(obj, ...) { ## takes obj created by makeCacheMatrix
        inv <-obj$getinv()         ## accesses inv stored in object     
        if(!is.null(inv)) {        
                message("getting cached data")
                return(inv)
        }
        data <- obj$get()  ## This part of the function is reached if inv was NULL
        inv <- solve(data, ...) ## solve for the inverse
        obj$setinv(inv)     ## set inv in object to be the new value
        inv  
}
