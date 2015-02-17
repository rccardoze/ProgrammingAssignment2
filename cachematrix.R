## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversed <<- inverse
  getinverse <- function() inversed
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inversed <- x$getinverse()
  if(!is.null(inversed)) {
    message("getting cached data.")
    return(inversed)
  }
  mike <- x$get()
  inversed <- solve(mike)
  x$setinverse(inversed)
  inversed
}
