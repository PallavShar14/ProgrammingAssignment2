##The specific work performed by the functions have been written close to the function code. The code exploites the scoping rules in R
#to minimize the computation and save time if the computation was already performed earlier by retrieving the previous value from the cache.

#The first function typically creates a special matrix and returns the list of set, get, setinverse and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse  
  getinverse <- function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function below calculates the inverse of special matrix created with the above function. It performs some 
#checks: it sees if the inverse is already available in the cache, then it would retrive the value. Else it performs computatiion necessary

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s))
  {
     message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
  
}
