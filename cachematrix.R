## Calcuating the inverse of a square matrix can be a resource heavy task. The 
## following functions create a special matrix that allows for the inverse
## to be cached so that it is only recalcuated if there is a change to the 
## initial matrix
## 


## makeCacheMatrix creates a special matrix accessed and manipulated by 
## functions that allow user to set the matrix (set), get the matrix (get), 
## invert the matrix (setInv) and retrieve a cached inverted matrix (getInv).

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function() inv <<- solve(x)
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# where x is a makeCacheMatrix object, cacheSolve function
# returns in the inverse of the matrix stored in x$get(). If this 
# hasn't already been inverted, inverts matrix and returns result,
# else retrieves the cached matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Getting cached inverted matrix")
        return(inv)
    }
    x$setInv()
    x$getInv()
}
    

