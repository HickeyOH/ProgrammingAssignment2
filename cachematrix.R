## These functions (makeCacheMatrix  and cacheSolve) cache the inverse of a matrix. 

## makeCacheMatrix creates a vector of four functions which set the value of the matrix, 
## get the value of the matrix, set the inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function () x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv= getinv)
}


## cacheSolve calculates the inverse of this matrix 

cacheSolve <- function(x, ...) {
        m<- x$getinv()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setinv(m)
        m
}
