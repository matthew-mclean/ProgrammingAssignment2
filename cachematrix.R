##makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
##it also stores a value i (initially NULL) that caches the inverse of the passed matrix
##it demonstrates the use of the '<<' operator to set the objects in their parent environment

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y ##<< is used to set the value of x in the scope of the containing function makeCacheMatrix (not function set)
    i <<- NULL ##<< is used to set the value of i in the scope of the containing function makeCacheMatrix
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse  ##<< is used to set the value of i in the scope of the containing function makeCacheMatrix
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix, however, it first checks 
##to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
}

## example usage:
##my_matrix = matrix( c(1, 2, 3, 1), nrow=2, ncol=2)
##x <- makeCacheMatrix(my_matrix)
##x$get()
##cacheSolve(x)
##cacheSolve(x) ##it will use the cached version this second time
##my_matrix = matrix( c(5, 2, 4, 6, 7, 1, 3, 5, 9), nrow=3, ncol=3)
##x$set(my_matrix)
## etc.