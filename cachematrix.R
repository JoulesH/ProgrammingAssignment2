## makeCacheMatrix produces a "matrix" with a series of functions that allow users to:
## 1. set a matrix
## 2. get a matrix (as in retrieve)
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL 
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL ## the matrix has changed, so reset this to NULL
  }
  
  ## returns the matrix
  get <- function() x
  
  ## sets the inverse matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse
  
  ## returns the inverse matrix
  get_inverse <- function() inv_matrix
  
  ## shows the list of possible internal function calls
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Return a matrix that is the inverse of 'x'
## First this will check to see if the inverse matrix has
## already been cached. If it has, then it returns the cached
## inverse matrix. If it has not, then it will compute the inverse
## matrix and cache it.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  
  ## The inverse matrix was not already cached, so
  ## grab the cached matrix and find its inverse below
  data <- x$get()
  
  # solve for the inverse matrix
  inv_matrix <- solve(data, ...)
  
  # cache the inverse
  x$set_inverse(inv_matrix)
  
  # return the inverse matrix
  inv_matrix
}
