##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than computing it repeatedly.

##Below pair of functions implement to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = numeric()){
  invs <- NULL
  set <- function(y) {
          x <<- y
          invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
        message("getting cached matrix")
        return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
  
##Test runs
##x <- rbind(c(1, 1/5), c(1/5, 1))
##m <- makeCacheMatrix(x)
##m$get()
##     [,1] [,2]
##[1,]  1.0  0.2
##[2,]  0.2  1.0
##cacheSolve(m)
##           [,1]       [,2]
##[1,]  1.0416667 -0.2083333

##cacheSolve(m)
##getting cached matrix
##[,1]       [,2]
##[1,]  1.0416667 -0.2083333
##[2,] -0.2083333  1.0416667
    
