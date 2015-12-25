## Caching the Inverse of a Matrix
## -------------------------------
## 1) makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
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


## 2) cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache. 
## Note that X is a square invertible matrix. 
## If not it will return the error message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  f <- function(chkm) class(try(solve(chkm),silent=T))=="matrix" 
          ## to check matrix is square matrix
  if(!f(data)) { 
    message("matrix is not sqare matrix. assign square matrix to x")
    return(m)
  }
  
  m <- solve(data)
  x$setsolve(m)
  m
}
