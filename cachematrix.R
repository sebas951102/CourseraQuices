

## this function works to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(inverse) inversa <<- inverse
  getinversa <- function() inversa
  list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}

## cachesolve returns the inverse of X, it search on the cached data, if i

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return an inverse matrix 
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached result")
    return(inversa)
  }
  datos <- x$get()
  inversa <- solve(datos, ...)
  x$setinversa(inversa)
  inversa
  
}
