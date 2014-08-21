
## makeCacheMatrix takes the input of the function and passes on the value of the inverse to m

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse      ##function(inverse) is called from cacheSolve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix. 
##And if statement is used so the function checks to see if an inverse was already calculated
## If it is already calculated the message "getting cached data" will appear
##If it is null then cacheSolve will calculate the inverse

cacheSolve <- function(x, ...) {
 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
