## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week 3 Assignment GitHub user: edward1two

##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
##get the value of the matrix, set the inverse Matrix and get the inverse Matrix. 
##The matrix object can cache its own object. 

makeCacheMatrix <- function(x = matrix()) 
{

inverseMatrix <- NULL
        
##set the value of the Matrix
setMatrix <- function(y) 
        {
	x <<- y
	inverseMatrix <<- NULL
		}
        
##get the value of the Matrix
 	getMatrix <- function() x
        
##set the value of the invertible matrix
	setInverse <- function(inverse) inverseMatrix <<- inverse
##get the value of the invertible matrix
	getInverse <- function() inverseMatrix
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

 cacheSolve <- function(x, ...) 
 {
##get the value of the invertible matrix from the makeCacheMatrix function
      inverseMatrix <- x$getInverse()
##if inverse matrix is not NULL
      if(!is.null(inverseMatrix))
	  {
      message("getting cached data")
      return(inverseMatrix)
      }
##else
      matrixData <- x$getMatrix()
      inverseMatrix <- solve(matrixData, ...)
      x$setInverse(inverseMatrix)
      return(inverseMatrix)
  }
