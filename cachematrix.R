## makeCacheMatrix get,set,get Inverse,set Inverse,get Inverse
## of the matrix.It uses <<-assignment operator to store the values of variables in the environment
 

makeCacheMatrix <- function(x = matrix()) {
  
    Inverse_Matrix <- NULL
    set <- function(y) 
      {
      x <<- y
      Inverse_Matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) Inverse_Matrix <<- inv
    get_inverse <- function() Inverse_Matrix
    
    ##returns a list of the above functions
  
   ## x$set(matrix1) ## to set the  matrix
    ##x$get ## to get that matrix
    ##x$set_inverse ## to set the inverse of matrix
    ##x$get_inverse ## to get the inverse of matrix
    list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)   

}


## calculates the inverse of matrix created from above function.t First checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the iverse of the matrix and sets 
##the value of the inverse in the cache via the set_inverse function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse() ##gets the inverse of matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##if not get the matrix
  m <- solve(data) ## compute the inverse of matrix
  x$set_inverse(m) ##set it to m
  m ## return result
}
