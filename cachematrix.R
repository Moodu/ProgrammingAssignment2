## Finds the inverse of a matrix.
## makeCacheMatrix initializes the matrix and returns utility functions
## cacheSolve received the list of utility functions and returns inverse of matrix.


## consuming code example::
## methods <- makeVector(matrix)
## matrix <- c(1,1,1,3,4,3,3,3,4)
## dim(matrix) = c(3,3)
## mmethods2 <- makeCacheMatrix(matrix)
## mmethods2$set(matrix)
## cacheSolve(mmethods2)

require("digest");   ## needed for matrix change compare
## install.packages("digest");
library(digest);

## method to set matrix and return list of functions


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  h <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    h <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## method to solver for inverse and use cache version of the matrix has not changed.
## matrix change is detected with MD5 HASH
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if((!is.null(m)) && (digest(x$get()) == h)) {
    message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  h <<- digest(data)  ##compute hash
  x$setinverse(m)
  m
}