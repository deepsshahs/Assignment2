makeCacheMatrix <- function(x = matrix()) {
# This function creates a special matrix objectx. then cacheSolve clacs the inverse of it. 
# if an inverse has been calulated it finds it and then retuens it without futher calculation. 
  m <- NULL
  set<- function(y) {
    x <<- y
    m<<- NULL
  }
  get <-function() x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getInverse()
  
  # This gets the inverse from the cache if it has been previously calculated or stored
  if(!is.null(m)){
    message("getting cahed data")
    return(m)
  }
  
  # Calculates the inverse of the matrix if its not in the cache
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
