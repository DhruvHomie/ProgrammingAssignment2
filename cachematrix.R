##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #setting the value of the matrix
  set <- function(y){
    x<<- y                #Access to variable outside the parent environment
    inv<<- NULL
  }
  
  #getting the value of the matrix
  get <- function () {x}
  
  #setting the value of the inverse
  setInverse <- function(inverse){
    inv<<- inverse
  }
  
  #getting the value of the inverse
  getInverse<- function() {inv}
  list(set=set, get=get, setInverse= setInverse, getInverse= getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  #This is to assign the inverse of X to inv
  inv<- x$getInverse()
  
  #Calculating is inverse is already calculated
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  #for new computation, we use solve
  mat<- x$get()
  inv<- solve(mat,...)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
