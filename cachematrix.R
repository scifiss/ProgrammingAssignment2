## function makeCacheMatrix creates a list of functions that
## set the value of the matrix in lexical scoping: set(x)
## get the value of the matrix in the cache: get()
## set the value of the inverse of the matrix into the cache: setinv(invx)
## get the value of the inverse of the matrix into the cache: getinv()
makeCacheMatrix <- function(x=matrix())
{
  # creates a special "matrix" object that can cache its inverse.
  
  inv <- NULL
  
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invx) inv <<- invx
  
  getinv <- function() inv
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## function cacheSolve read the output of makeCacheMatrix
cacheSolve <- function(x, ...)
{
  # computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  ## return a matrix that is inverse of x
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  inv <- solve(data, diag(nrow(data)))
  x$setinv(inv)
  inv
  
}