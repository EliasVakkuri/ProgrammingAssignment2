## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        setMat <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        getMat <- function () {
                x
        }
        
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        getInv <- function() {
                inv
        }
        
        list (setMat = setMat, getMat = getMat,
              setInv = setInv, getInv = getInv)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        
        if (!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)
        }
        
        a <- x$getMat()
        
        inv <- solve(a)
        
        x$setInv(inv)
        
        inv
}
