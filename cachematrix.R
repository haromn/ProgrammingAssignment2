## we define two functions: 'makeCacheMatrix' and 'cacheSolve'
## We use the environment of 'makeCacheMatrix' to store a matrix and its inverse 
## We have sub functions to read and write these values and we use the
## assignment operator <<- to assign the value in the scope of the 'makeCacheMatrix'
## function instead of the sub function environment. 
## The second is used to get the inverse of the parameter 
## (an "instance" of 'makeCacheMatrix' ex. Matrix <- makeCacheMatrix(matrix(1:4,2,2)) )
## by first checking if there is a stored value or computing it if there is none.


## This function has 'x' and 'inverse' matrix variables in its environment,
## and four functions to set and get the values.

makeCacheMatrix <- function(x = matrix()) {
    
  inverse <- NULL
    
  set <- function(y = matrix()) {
    x <<- y
    inverse <<- NULL
  }
      
  get <- function() {
    x
  }
      
  setinverse <- function(i) {
    inverse <<- i
  }
      
  getinverse <- function() {
    inverse
  }
    
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function reads the value of 'inverse' from the environment
## of formal parameter 'm' using the 'getinverse' function.
## First we test if it has a numerical value in that case we are done
## and we return that value. 
## Otherwise we compute the inverse and save the value using 'setinverse' of 'm'.
## Finally we return the value of the inverse as well.

cacheSolve <- function(m, ...) {
        
  i <- m$getinverse()
  if (!is.null(i)) {
    message("Getting matrix inverse from the cache.")
    return(i)
  }
  
  i <- solve(m$get())
  m$setinverse(i)
  return(i)
  
}
