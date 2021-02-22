## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeMatrix creates a special "matrix", 
##which is really a list containing a function to

#set the values of the matrix
#get the values of the matrix
#set the values of the inverse mean
#get the valuse of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
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

 
## Write a short comment describing this function
##cacheSolve gets the inverse of the matrix 
##it first checks to see if the inverse has already been calculated. If so, 
##it gets the inverse from the cache and skips the computation
##otherwise this function calculates calculates the inverse of the data and sets the inverse 
##in the cache via the  setinverse function.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
