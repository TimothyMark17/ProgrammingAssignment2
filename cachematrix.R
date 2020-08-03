## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
          x <<- y
          inv <<- NULL
    }
    get <- function() {x}                               ## Getting the value of the matrix
    setInverse <- function(inverse) {inv <<- inverse}   ##Setting the value of the inverse
    getInverse <<- function() {inv}                     ##Getting the value of the inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





## Write a short comment describing this function

## Computes the inverse of a matrix; matrix gotten from the functions above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()                         
      if(!is.null(inv)){                  ##To determine if the inverse has been calculated and cached
          message("Getting cached data")
          return(inv)                     ##Returns the inverse
      }
      mat <- x$get()          
      inv <- solve(mat, ...)  ## Gives inverse of mat    
      x$setInverse(inv)        ## Setting the value of the result in the cache
      return(inv)             ## Finally returns inv
}
