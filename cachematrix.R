## makeCacheMatrix is a function which creates a matrix object
## that can cache its inverse
## cacheSolve is a function which takes a matrix and returns/caches its inverse

## makeCacheMatrix reads in a matrix from the user
## and uses m as a placeholder for the inverse, m is set to null when program runs
##user can pass a matrix using the makeCacheMatrix function or setmatrix subfunction
## user can also use setinverse to set the inverse as any matrix the user wants
## subfunction getinverse will also return null or the inverse if cached


makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL  
  setmatrix <- function(y) { 
    x <<- y ##if user uses setmatrix, the new matrix value is passed from y to x
    m <<- NULL ##placeholder set to null again if user uses setmatrix
  }
  getmatrix <- function() x ##allows user to view the current matrix value
  setinverse <- function(solve) m <<- solve ##allows user to set inverse of current matrix value
  getinverse <- function() m ##allows user to get value of current inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

## cacheSolve is a function which takes a matrix and returns/caches its inverse
## if the matrix changes, cacheSolve will compute its inverse. 
## if m is null (or if the matrix has not changed), cacheSolve will return the
## cached inverse of the original matrix

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
