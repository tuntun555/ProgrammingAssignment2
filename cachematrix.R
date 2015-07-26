## This two functions calculate inverse of a matrix 
## makeCacheMatrix sets a value of a matrix
## cacheSolve gets value of a matrix from makeCacheMatrix, calculates
## inverse and chaches the result 

## This function takes a matrix as an argument and it is assinged
## to a variable so it can be used by cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve function calculates inverse of a matrix and
## saves the result, if inverse was calculated before it 
## returns cached result

cacheSolve <- function(x, ...) {
  
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  
  
}
