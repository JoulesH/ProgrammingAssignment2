## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL 
  
  ## set the matrix as an environment variable
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL ## the matrix has changed, so reset this to NULL
  }
  
  ## retrieving the cached matrix
  get <- function() x
  
  ## sets the inverse of the matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse
  
  ## returns the matrix
  get_inverse <- function() inv_matrix
  
  ## shows the list of possible internal function calls
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  # grab the cached matrix and find its inverse below
  data <- x$get()
  
  # since we haven't computed it yet, solve for the inverse matrix
  inv_matrix <- solve(data, ...)
  
  # cache the inverse
  x$set_inverse(inv_matrix)
  
  # return the inverse matrix
  inv_matrix
}
