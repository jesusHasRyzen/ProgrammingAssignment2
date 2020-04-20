
makeCacheMatrix <- function( dataMatrix= matrix())
{
  matrixInverse = NULL
  set<- function(x){
    dataMatrix <<- x
    matrixInverse <<- NULL
  }
  get <- function() dataMatrix
  setInverse <-function(tempInverse) matrixInverse <<- tempInverse
  getInverse <- function() matrixInverse
  list(set = set, get= get, setInverse = setInverse, getInverse=  getInverse)
}
cacheSolved <-function(x,...)
{
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