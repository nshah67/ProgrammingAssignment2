#Neha Shah Nov 11, 2018
#This function aims to cache potentially time-consuming computations, specifically 
#the computation to invert a matrix



## The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a function to 1)set the value of the vector
# 2) get the value of the vector, 3) set the value of the inverse matrix and finally
# 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'; first checks if already stored, then
#retreives from cache, otherwise calculate inverse and sets the value in the cache via
#the setinverse function

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
