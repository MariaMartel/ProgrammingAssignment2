##Assignment: Caching the Inverse of a Matrix.
##The goal is to write a pair of functions that cache the inverse of a matrix (to avoid costly computation).

##The first function, makeCacheMatrix, creates a special "matrix" object that can cache its 
##inverse, which is really a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The second function, cacheSolve, computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

##To test the two functions, we need to define previously an invertible matrix, A, e.g.:
##A<-matrix(c(1,0,1,-2,3,4,0,-2,1),nrow=3,ncol=3) (with solve(A) we can obtain its inverse)
##and then apply makeCacheMatrix to it, afterwards define B<-makeCacheMatrix(A), and apply cacheSolve(B).
##cacheSolve calculates the inverse for the first time, and from the cache next.
