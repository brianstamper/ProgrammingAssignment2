## The following functions provide a matrix with
## a cached calculation of the inverse.
 
## Usage example, with a random matrix:

## First, let's create a random matrix, look at it,
## and then look at its inverse the traditional way
## using the solve() function.

# > mat <- matrix(rnorm(9), 3, 3)
# > mat
# [,1]       [,2]       [,3]
# [1,] -1.0853849  0.8721036  0.6051487
# [2,]  0.9996967 -1.5262710  0.9950324
# [3,]  0.1664028 -0.3034187 -1.2819075
# > solve(mat)
# [,1]       [,2]       [,3]
# [1,] -1.85250169 -0.7663982 -1.4693961
# [2,] -1.18698493 -1.0586724 -1.3820932
# [3,]  0.04048029  0.1510953 -0.6436959
 
## Now, we form a matrix with caching, and then show how
## to look at it and its inverse.
# > cmat <- makeCacheMatrix(mat)
# > cmat$get()
# [,1]       [,2]       [,3]
# [1,] -1.0853849  0.8721036  0.6051487
# [2,]  0.9996967 -1.5262710  0.9950324
# [3,]  0.1664028 -0.3034187 -1.2819075
# > cacheSolve(cmat)
# [,1]       [,2]       [,3]
# [1,] -1.85250169 -0.7663982 -1.4693961
# [2,] -1.18698493 -1.0586724 -1.3820932
# [3,]  0.04048029  0.1510953 -0.6436959

## And we see that if we request the inverse again
## it uses the cached values.
# > cacheSolve(cmat)
# getting cached data
# [,1]       [,2]       [,3]
# [1,] -1.85250169 -0.7663982 -1.4693961
# [2,] -1.18698493 -1.0586724 -1.3820932
# [3,]  0.04048029  0.1510953 -0.6436959
 
 
## makeCacheMatrix is a function which takes an ordinary
## matrix as its input and returns a list object which
## contains functions for getting and setting the matrix
## as well as the matrix inverse.

## cmat <- makeCacheMatrix(mat) constructs cmat using matrix mat
## cmat$set(y)       sets the matrix to equal y
## cmat$get()        returns the matrix
## cmat$setsolve(s)  sets the inverse of the matrix to equal s
## cmat$getsolve()   returns the inverse of the matrix

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


## cacheSolve is a function that takes a makeCacheMatrix list
## and returns the inverse of the matrix it represents. If
## the inverse has not been calculated already, it calculates
## it and stores the result. If the inverse had already been
## calculated, it returns the cached value.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
