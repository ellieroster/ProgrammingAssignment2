## cachematrix - two functions used to computer the inverse of a matrix. To save
## time, the inverse is stored, and used, in cache.
## makeCacheMatrix returns a list of four functions: set, get, getinv,setinv
## These functions create an environment into which the inverse of a matrix can be stored
##
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invmatrix <<- solve(x)
  getinv <- function() invmatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##
## cacheSolve - gets the inverse of a matrix. If the inverse exists in cache, it is used, else, 
## it creates the inverse of the matrix and sets the value in cache via the setinv function. 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
  
