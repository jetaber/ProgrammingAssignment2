## NOTE: This script only works with matricies that have an inverse which can be 
## determined by the solve() method.
##
## Stores the inverse of a matrix in cache (memory) using solve.  If the same matrix is
## provided to the cachesolve function it retrieves the inverse from the cache as opposed
## to recalculating it.  

## Caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## checks if inverse is in cache, if not run solve and store in cache, if so, get from cache
cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   
   #Gruesome, yes?  There is a requirement to see if the matrix that was used
   #to generate the inverse is the same as the new matrix, then return the cached
   #matrix, if I understood this enough, I would have just stored the original 
   #matrix in the makeCacheMatrix function, but I am pretty clueless at this point.
   #However, by not storing the original, you can cut down on memory usage
   #It is a use-case analysis whether is is more acceptable to store the original
   #and not compute the identity or do not store the original and compute the identity
   if ((!is.null(i)) && (x$get() %*% (x$get() %*% i)) == x$get())  {
      data <- x$get()
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}