## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##function creates a special matrix with a list containing the abilities to set get both the matrix itself and its inverse,
## althought he inverse is initailly NULL
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse = NULL
  set<- function(y){
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <-function(tempInverse) matrixInverse <<- tempInverse
  getInverse <- function() matrixInverse
  list(set = set, get= get, setInverse = setInverse, getInverse=  getInverse)
}


## Write a short comment describing this function
##function passes a special matrix to get the inverse of the matrix usiing the 
##solve() function and returns the inverse  matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("getting cached matrix")
    return(inverse)
  }
  tempData <- x$get()
  inverse <- solve(tempData)
  x$setInverse(inverse)
  inverse
}
