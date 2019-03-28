###This file contains two functions the makeCacheMatrix and cacheSolve.  The first function,
#makeCacheMatrix creates an R object that stores a matrix (x) and its inverse (m).  The second 
#function, cacheSolve, requires the argument that is returned by the first function to determine the
#the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # This function initializes x and m
  set <- function(y) { # This assigns the input argument (y) to the x object in the parent environment
    x <<- y            #It also assigns the NULL value to m in the parent environment clearing any
    m <<- NULL         #clearing any previous values of m.
  }
  get <- function() x  #This retrieves x from the parent environment
  setinv <- function(inv) m <<- inv #assigns m to the parent environment
  getinv <- function() m # This retrieves m from the parent environment
  list(set = set, get = get, #creates a list for the cacheSolve function to retrieve
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  m <- x$getinv()  #retrieves m from the makeCacheMatrix function
  if(!is.null(m)) { #checks to see if m is null if so it returns the m value from makeCacheMatrix
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) #calculates the invers of the matrix
  x$setinv(m)
  m #prints the inverse of the matrix
}

## Example showing the same outputs by using the solve function and the cacheSolve/makeCacheMatrix 
#function

mat<-matrix(c(1,5,7,8),2,2)
solve(mat)
cacheSolve(makeCacheMatrix(mat))