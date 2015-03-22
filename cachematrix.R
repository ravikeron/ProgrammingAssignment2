## The following pair of functions cache the inverse of a matrix 
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  ## Check if the inverse of the matrix is already available
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m       
}
