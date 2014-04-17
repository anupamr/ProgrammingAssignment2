## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() takes a matrix,x, as input and returns a list with set,get,setinv 
## and getinv functions defined on the input matrix x. In effect this computes 
## a special matrix with capability to cache the calculated matrix inverse, to avoid  
## costly computation on same matrix multiple times. Note: if underlying matrix 
## changes using set(y), inverse is set to NULL for recalculation on later calls to 
## cacheSolve() function defined below.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize matrix inverse to NULL
  inv <- NULL
  ## set the matrix object in special matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix object from sepcial matrix
  get <- function() x
  
  ## set the matrix inverse in the special maarix; 
  setinv <- function(inverse) inv <<- inverse
  
  ## get the matrix inverse from special matrix
  getinv <- function() inv
  
  ## return a list of functions (set,get,setinv,getinv)
  ## based on matrix x passed to makeCacheMatrix()
  
  list(set = set, get = get , setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve() returns the inverse of a matrix. 
## cacheSolve(x,...) : x is a special matrix calcualted earlier using the makeCacheMatrix()
## computes inverse only if it's not already cached in x. If it's cached that value is 
## returned. If it's not cached then inverse is calculated using solve() function and
## computed value is cached for future usage. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  
  ## check if the special matrix, x, already has the inverse calculated
  inv <- x$getinv()
  
  ## if inverse already calculated return that value
  if(!is.null(inv)) {
    message("getting cached inverse value")
    return(inv)
  }
  
  ## inv == NULL ; if you reached here inverse is not cached in special matrix
  ## get the matrix object from special matrix, x.  
  matrix <- x$get()
  
  ## calculate the inverse
  inv <- solve(matrix)
  
  ## now cache the calculated inverse into special matrix
  x$setinv(inv)
  
  ## return the inv
  inv
}
