### Assignment: Caching the Inverse of a Matrix

##`makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # value of inverse of the matrix will be stored in xInv
  xInv<-NULL
  
  # 1. set the value of the matrix  
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  # 2. get the value of the matrix
  get <- function() {
    x
  }
  
  # 3. set the value of inverse of the matrix
  setinverse <- function(i) {
    xInv<<- i
  }
  
  # 4. get the value of inverse of the matrix
  getinverse <- function() {
    xInv
  }
  
  # Return the special matrix
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return the cached inverse
  xInv <- x$getinverse()
  
  if(!is.null(xInv)) {
    message("getting cached inverse")
    return(xInv)
  }
  
  # Return a matrix that is the inverse of 'x'
  data <- x$get()
  xInv <- solve(data, ...)
  x$setinverse(xInv)
  xInv
  
}

## Sample run

# xInv<-makeCacheMatrix(matrix(c(3,7,1,2),2,2))

# cacheSolve(xInv)     
##        [,1] [,2]
##  [1,]   -2    1
##  [2,]    7   -3

# cacheSolve(xInv) 
## getting cached inverse
##        [,1] [,2]
##  [1,]   -2    1
##  [2,]    7   -3