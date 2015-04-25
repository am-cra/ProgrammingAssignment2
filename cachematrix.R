## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix : Creates a 'special' matrix object that 
##                   can cache its inverse
##    Input  : a matrix (Optional, default value = empty matrix)
##    Cache  : A matrix (x) and its inverse (i) are stored 
##             in the local cache
##    Output : a 'special' matrix object which is a 
##             list of 4 functions which can maipulate the local cache
##             get        : Returns the current stored matrix
##             set        : Takes as input a matrix, and sets x 
##                          to this matrix
##             getinverse : Returns the inverse matrix
##             setinverse : Takes as input a matrix, and sets i 
##                          to this matrix
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
## cacheSolve: Computes the inverse of the 'special' matrix object 
##             returned by makeCacheMatrix
##        Input : A 'special' matrix object returned by makeCacheMatrix
##        Output: The inverse of the matrix. If the inverse has already 
##                been set then the cached inverse is returned, else 
##                it is calculated using the solve function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
