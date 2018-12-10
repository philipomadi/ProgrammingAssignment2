## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix is very similar to the makeVector() example

makeCacheMatrix <- function(x = matrix()) {
  #variable to hold inverse of matrix x, NULL by default
  matrix.inverse <- NULL
  #fuction to set value of matrix
  setmatrix <- function(y) {
    x <<- y
    matrix.inverse <<- NULL
  }
  #function to get passed matrix
  getmatrix <- function(){
    x
  } 
  #funtion to cache computed inverse of matrix 
  setinverse <- function(inverse){
    matrix.inverse <<- inverse
  }
  #function to get cached inverse of matrix
  getinverse <- function() {
    matrix.inverse
  }
  #return list of funtions
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## also similar to cachemean() in the example

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #collect cached inverse
  matrix.inv = x$getinverse()
  
  # if cached
  if (!is.null(matrix.inv)){
    # return cached inverse 
    return(matrix.inv)
  } else {
    #its a new matrix, calculate inverse
    new.matix = x$getmatrix()
    matrix.inv = solve(new.matix, ...)
    #cache it
    x$setinverse(matrix.inv)
    return(matrix.inv) 
  }
}
