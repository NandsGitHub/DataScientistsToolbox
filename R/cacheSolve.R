cacheSolve <- function(x, ...) {
  
  mat_inv <- x$getMatrixInverse()
  
  ##Checking if inverse is already cached
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  ##Computing the inverse of the matrix since its not in cache
  data <- x$get_matrix()
  mat_inv <- solve(data, ...)
  
  ##Caching the inverse
  x$setMatrixInverse(mat_inv)
  mat_inv
}