## Programming Assignment 2: Caching the Inverse of a Matrix

## Per the stated instructions, the following code 
## contains two functions: one to create a "matrix" object
## that can cache in its inverse, and the second to compute
## the inverse of the special "matrix".

## First, create the "matrix" object that can cache in its inverse:

makeCacheMatrix <- function(x = matrix()) {
            vers <- NULL
            set <- function (y) {
              x <<- y
              vers <<- NULL
            }
            get <- function () x
            setinv <- function(inverse) vers <<- inverse
            getinv <- function () vers
            list(set = set, get = get,
                  setinv = setinv, getinv = getinv)
}


## Next, use the "solve" function to compute the inverse of the matrix

cacheSolve <- function(x, ...) {
        vers <- x$getinv()
        if (!is.null(vers)) {
            message("getting cached data")
            return(vers)
        }
        
        mat <- x$get()
        vers <- solve(mat, ...)
        x$setinv(vers)
        vers
}

##STOP! End of functions##

##As a final step, test functions makeCacheMatrix and cacheSolve:

#Test A: Test where inverse should exist:

matrix_a <- makeCacheMatrix(matrix(data = 1:4, nrow = 2, ncol = 2))
matrix_a$get()
cacheSolve(matrix_a) #confirmed, inverse result is returned

#Test B: Test where inverse does not exist (i.e. on a singular matrix):

matrix_b <- makeCacheMatrix(matrix(data = c(3, 6, 5, 10), nrow = 2, ncol = 2))
matrix_b$get()
cacheSolve(matrix_b) #confirmed, error in solve.default as matrix is singular