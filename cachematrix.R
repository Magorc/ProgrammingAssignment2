## These functions first create a "matrix object" which is made
## up of a list of functions that cache the inverse of a pre-
## created square matrix. Then we call cacheSolve to check to 
## see if the inverse has already been created. If it has, no
## insertion is made, if it hasn't the inverse solved in the 
## first function is cached.

## This function creates the list of functions to get the value
## of the matrix, set the value to a local variable, get the 
## inverse of that local variable, then store that in a global
## variable

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #setting the pre-made matrix to a global
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- q
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Looks to see if there's something in the cache. 
## Prints "getting cached data" if it's full. If not full
## stores the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
