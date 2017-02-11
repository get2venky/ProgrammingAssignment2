## R Programming Week3, Programming Assignment 2: Lexical Scoping
## Author: Venkatesh Vedam

##Function 1 - makeCacheMatrix
##Creates a special "matrix" object that can cache its inverse.
##Input - Inversible Matrix object
##Output - list of functions to set and get values

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##Function 2 - cacheSolve
##Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##the function retrieves the inverse from the cache.
##Input - Vector object created by makeCacheMatrix
##Output - Inverse of the matrix contained in the vector object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
