makeCacheMatrix <- function(x = matrix()) {
  ##Defining the matrix mat_inv for caching
  mat_inv <- NULL
  
  ##Caching the matrix
  set_matrix <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  ##Getting the matrix from cache
  get_matrix <- function() x
  
  ##Caching the inverse of the matrix
  setMatrixInverse <- function(inv) mat_inv <<- inv
  
  ##Getting the inverse from cache
  getMatrixInverse <- function() mat_inv

  list(set_matrix = set_matrix, get_matrix = get_matrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}