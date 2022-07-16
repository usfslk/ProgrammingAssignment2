makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(x) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- getInverse(x)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- get(x)
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
