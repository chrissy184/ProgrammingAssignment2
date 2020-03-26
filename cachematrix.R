##function creates a matrix and caches the inverse for reuse
##function sets matrix, retrieves value of matrix and sets, gets inverse
makeCacheMatrix <- function(x = matrix()) {
 ##initialize m and x
  ## set a default value for x
  m <- NULL
  ##assign input argument to x in parent environment
  ## assign null value to m in parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##define getter for matrix x
  get <- function() x
  ## define setter for inverse of matrix x
  ##assign input argument to value of m in parent environment
  setinverse <- function(inverse) m <<- inverse
  ##define getter for inverse m
  getinverse <- function() m
  ##assign each function to named element in list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##function to compute matrix inverse or return cached value if 
##it's not a new matrix
cacheSolve <- function(x, ...) {
  ##retrieve inverse from object x
  m <- x$getinverse()
  ##check to see if there is a cached inverse or if inv is null
  ##if not a new matrix, return cached inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if null m, then retrieve matrix object
  data <- x$get()
  ##calculate inverse for matrix and assign to m
  m <- solve(data, ...)
  ##return inverse of matrix to parent environment 
  x$setinverse(m)
  m
}
