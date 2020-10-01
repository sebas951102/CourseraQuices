

## this function works to cache a matrix called X

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y ##cache
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(inverse) inversa <<- inverse 
  getinversa <- function() inversa
  list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}

## cachesolve returns the inverse of X, it search on the cached data, if the inverse matrix is cached
## it just return that, if not, it calculates and return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return an inverse matrix 
  
  inversa <- x$getinversa() #search the inverse
  if(!is.null(inversa)) {  #so the matrix is chached
    message("getting cached result")
    return(inversa)       #and it ends
  }
  datos <- x$get()
  inversa <- solve(datos, ...) #if its not cached
  x$setinversa(inversa)
  inversa
  
}
