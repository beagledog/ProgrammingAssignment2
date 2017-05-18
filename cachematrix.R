## This is Assignment 2 of the R Programming course
## for the Johns Hopkins Data Science Specialization.
## The main purpose of the two functions makeCacheMatrix and 
## cacheSolve, is to understand lexical scoping.

## This function creates a special "matrix" object 
## that can cache its inverse.
## The function begins with an empty matrix, 'x'.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  
## Define getter for matrix 'x'
  get <- function() x

## Define setter for inverse 'inv'
  setSolve <- function(solve) inv <<- solve

## Define getter for inverse 'inv'
  getSolve <- function() inv
  
## Define the list of named elements, the nested functions.
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check to see if the cache is not null
  inv <- x$getSolve()
  if(!is.null(inv)) {
    ## If cache is not null, return cache value 'inv'
    message("getting cached inverse")
    return(inv)
  }
  ## If cache is null, solve new inverse 'inv_new'
  data <- x$get()
  inv_new <- solve(data, ...)
  x$setSolve(inv_new)
  inv_new
}
