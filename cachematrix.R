## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function sets and gets the value of a matrix m and then sets and gets
## the value of the inverse of the matrix m.
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The following function computes the inverse of the special 
#"matrix" returned by the makeCacheMatrix above. However, it first 
#checks to see if the inverse has already been calculated. 
#If so, it retrieves the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data matrix and sets the value of 
#the inverse in the cache via the setinv function.

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
