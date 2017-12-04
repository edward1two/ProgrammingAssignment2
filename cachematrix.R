## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week 3 Assignment GitHub user: edward1two

makeCacheMatrix <- function(x = matrix()) 
{

inv <- NULL
        
##set the value of the Matrix
set <- function(y) 
        {
	x <<- y
	inv <<- NULL
		}
        
## Get the value of the Matrix
 	get <- function() x
        
## Set the value of inverse of the matrix
	setInverse <- function(inverse) inv <<- inverse
## Get the value of inverse of the matrix 
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

 cacheSolve <- function(x, ...) 
 {
##get the value of the invertible matrix from the makeCacheMatrix function
      inv <- x$getInverse()
##if inverse matrix is not NULL
      if(!is.null(inv))
	  {
      message("getting cached data")
      return(inv)
      }
##else
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      inv
  }
