## Assignment 2: Caching the Inverse of a Matrix
## Student-Name: Lan Ngo
## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache 
##its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
      x <<- y
      invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    invmatrix <- x$getinverse()
    ## Return a catched inverse matrix if it is already calculated before and stop here
    if(!is.null(invmatrix)){
        message('getting catched inverse matrix.')
        return(invmatrix)
    }
    ## if the invmatrix is null, get the value of the matrix
    data <-x$get()
    ##calculate the inverse of the matrix by solve function
    invmatrix <- solve(data)
    x$setinverse(invmatrix)
    invmatrix
}
