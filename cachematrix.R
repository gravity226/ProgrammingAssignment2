## Tommy Martin
## Saturday May 23, 2015
## The function will accept an invertable matrix and then ouput the inverse.

## The makeCacheMatrix function needs to have a matrix passed into it upon calling
## it (ex. m <- matrix(sample(1:9), nrow=3, ncol=3) makeCacheMatrix(m))
## The matrix passed into the function needs to be square meaning that it has to have
## the same number of rows as columns.  

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


## This function will cache the matrix used in the previous function and then ouput
## the inverse of the matrix (if possible).  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## The code for the testInvertible function was found at:
  ## http://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
  testInvertible <- function(m) class(try(solve(m)))=="matrix"
  
  if (testInvertible(data)) {
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }
  else {
    "An invertible matrix needs to be passed to this function"
  }
}
