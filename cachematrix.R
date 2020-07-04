## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## returns a list of functions to
## set the matrix, get the matrix, set the inverse of the matrix and get the inverse of the matrix
## this output list can be used as input to cacheSolve() to cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
          matrx <- NULL
          set <- function(y) {
                    x <<- y   ##<<- operator which can be used to assign a value to an object in an 
                              ##environment that is different from the current environment
                    matrx <<- NULL
          }
          ##function that returns the generated matrix, with no arguments
          get <- function() x 
          
          ##function used to set values of matrix received from cacheSolve()
          setinv <- function(inverse) inv <<- inverse
          
          ##functions returns inverse of the matrix, with no arguments
          getinv <- function() inv 
          
          ##list of functions, can be sent to cacheSolve() as input
          list(set = set , get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function


## function is used to calculate inverse of the matrix which was given to makeCacheMatrix as original input
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) { ##check for missing/invalid values
                    message("retreiving cache")
                    return(inv)
          }
          matrix_data <- x$get() ##get the matrix or the original input matrix to makeCacheMatrix()
          inv <- solve(matrix_data, ...) ##calculate/solve the inverse for the matrix
          
          x$setinv(inv) ## set the inverse corresponding value in the cache using the setin()
          inv ##return the completed inverse matrix with inverse values of the original matrix
}
