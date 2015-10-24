## This function creates a special "matrix" object (list)
## with 4 funcs:
## set: saves the matrix
## get: returns saved matrix
## setInverse: saves inversed matrix
## getInverse: returns inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  savedInv <- NULL
  
  set <- function(new_matrix) {
    x <<- new_matrix
    savedInv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    savedInv <<- inverse
  }
  
  getinverse <- function() {
    savedInv
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns inversed matrix using cached value if it exists.
## Computes inversed matrix and caches it if no cached value exists.
## Input param x - result of makeCacheMatrix(matrix) call.

cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  
  if (!is.null(matrix_inverse)) {
    message("cached")
    
    return(matrix_inverse)
  }
  
  cur_matrix <- x$get()
  
  matrix_inverse <- solve(cur_matrix)
  x$setinverse(matrix_inverse)
  
  matrix_inverse
}


## sample run
## x <- rbind(c(1,2), c(3,4))
## mtx <- makeCacheMatrix(x)
##
## first run -> computes and saves
## cacheSolve(mtx)
## 
## second run -> cache hit
## cacheSolve(mtx)