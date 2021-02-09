## Two function that cache the inverse of a matrix when given


## Creates a matrix

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
}


## Solves the inverse of matrix above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("Getting cached inverse")
    return(i)
    
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}