##This function creates an object in closed environment 
##that generates a list with various methods necesary to handle caching of matrix inversion 

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initalize object to collect matrix inverse
  inv <- NULL
  
  ## sub-mehod to set handling "matrix" object, if input matrix has to be changed
  ## inverse is also reset 
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## sub-method to get "matrix" object stored 
  getmatrix <- function() x
  
  ## sub-method to set inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  
  ## sub-method to get the inverse
  getinverse <- function() inv
  
  
  ## construct and collect the object as a list
  ## reset the class
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
  
}

## Return a matrix that is the inverse of 'x'
## assumes matrix is square and invertable to use with solve()

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    system.time(return(inv))
  }
  
  ## get the input matrix
  inimatrix <- x$getmatrix()
  
  ##calculate the matrix inverse
  inv <- solve(inimatrix)
  
  ##store the matrix inverse in the collection object
  x$setinverse(inv)
  
  system.time(return(inv))
}
