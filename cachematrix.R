## R Programming, assignment 2: Caching.
## Functions for creating and using a matrix that can cache its inverse to the
## global environment to avoid unnecessary recalculations.

## Function makeCacheMatrix
## Arguments:
##      x = matrix(): The matrix to use as base.
## Outputs:
##      - Matrix cached to the global environment 
##      - List of functions to manipulate the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # setMat: set the matrix
        # If a new matrix is given, the inverse needs to be reacalculated
        # --> set to NULL
        setMat <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        # getMat: get the matrix
        getMat <- function () {
                x
        }
        
        # setInv: set the matrix inverse
        setInv <- function(inverse) {
                inv <<- inverse
        }
        
        # getInv: get the inverse
        getInv <- function() {
                inv
        }
        
        # Return the list of functions to manipulate the matrix cached to the
        # global environment
        list (setMat = setMat, getMat = getMat,
              setInv = setInv, getInv = getInv)
        
}

## Function cacheSolve
## Arguments:
##      x: "Cache matrix" = list of functions that point to the original
##         matrix. Created with makeCacheMatrix.
## Functionality: Either retrieves the already calculated inverse for memory
##      or calculates the inverse using 'solve' and stores it. Uses the 
##      functions defined in 'makeCacheMatrix'.
## Outputs:
##      inv: The inverse of the original matrix, either retrieved from the
##           cache or calculated with the 'solve' function.

cacheSolve <- function(x, ...) {
        
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
