## These two functions will cache the inverse of a matrix

## This function will create a matrix object and caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
          x<<-y
          m<<-NULL
  }
  get<-function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix )
}


## This function will compute the inverse of the matrix created above or 
## it will retrieive the cached version of the inverse (if it has already
## been computed). It will return m, which now contains the inverse.

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
          message("getting cached data")
          return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}