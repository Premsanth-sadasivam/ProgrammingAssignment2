## To store the data in a cache to retrive it without any computation when ever it is need the next time
## function do create a cache and checks for the value that has been calculated earlier

## This function takes the given matrix and converts in to a special object with a space for memory in a different environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  inverse = NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  set_inv <- function(inv) inverse <<- inv
  get_inv <- function() inverse
  
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)

}


## check for the inverse value that is computed earlier and if it is s new matrix then it 
##calculates the inverse of a matrix and stores it in the space which is created in the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  y <- x$get_inv()
  if(!is.null(y))
  {
    message("printing from cache")
    return(y)
  }
  
  y <- x$get()
  inv <- solve(y)
  x$set_inv(inv)
  inv
}
