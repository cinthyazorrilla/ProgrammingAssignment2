## Create the matrix

makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

xvec <- matrix( c(5, 1, 0,
                  3,-1, 2,
                  4, 0,-1), nrow=3, byrow=TRUE)

makecachematrix.object <- makecachematrix(xvec)
ls(envir = environment(makecachematrix.object$set))
cachesolve(xvec)#cannot make it work