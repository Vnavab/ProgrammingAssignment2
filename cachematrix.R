## Functions makeCacheMatrix and cacheSolve allow to create the a special matrix object,
## caching its inverse and thus saving time in future computation
## wARNING: The matrix supplied has to be invertible

## This function creates a list from a matrix where inverse matrix will be cached

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setmxi <- function(inv) mx <<- inv
  getmxi <- function() mx
  list(set = set, get = get,
       setmxi = setmxi,
       getmxi = getmxi)
}


## This function recovers a inverse matrix (mx) cached in x. If mx doesn't exist, 
## an inverse matrix is created with solve() and cached in x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mx <- x$getmxi()
  if(!is.null(mx)) {
    message("getting inverse matrix in cache")
    return(mx)
  }
  mx <- solve(x$get(),...)
  x$setmx(mx)
  mx
}