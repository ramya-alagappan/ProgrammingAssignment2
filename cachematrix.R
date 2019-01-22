# Function that creates the matrix with caching 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInv <- function(solve) m <<- solve (x)
  getMatrixInv <- function() m
  list(set = set, get = get,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv)
}

# function that returnsd the inverse of the matrix. Uses the cahe if it is the same as he previous call
cacheSolve <- function(x, ...) {
  m <- x$getMatrixInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("no cached data")
  data <- x$get()

  m <- solve(data)
  x$setMatrixInv(m)
  m
}