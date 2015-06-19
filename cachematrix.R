#Siddharth P Kulkarni's cachematrix code: These functions allow for a very
#efficient processing that calculates the inverse of a matrix using the base R
#solve() function. This stores matrixes in cache so as not to take a lot of
#memory in the R environment

#This function creates a list of functions, checking at first to see if the matrix
#has already been calculated and stored in the cache
makeCacheMatrix <- function (x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(mean) m <<- mean
  getmatrix <- function() m
  list (set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

#This function takes the result from the makeCacheMatrix and uses the cached matrix (if it exsts)
#and then using that cached matrix as the input to solve the inverse
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
