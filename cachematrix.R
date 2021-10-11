## Put comments here that give an overall description of what your
## functions do :

# Matrix inversion is usually a costly computation there is a benefit that caching
# the inverse of the matrix is more efficient that computing it repeatedly.

## Write a short comment describing this function
#  makeCacheMatrix: This function creates a special “matrix” object that can cache 
#  its inverse. 
#  cacheSolve:This function computes the inverse of the special “matrix” returned 
#  by makeCacheMatrix above

 makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL            
   set <- function(y) {
     x <<- y
     i <<- NULL     
     
   }
     
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
 }

 #This function computes the inverse of the special “matrix” returned by 
 #makeCacheMatrix above.  
    
 cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if (!is.null(i)) {
     message("getting data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
 }
 # TRIAL TESTING
      
 M <- matrix(c(2,4,6,8),2,2)
 
 #solve(M) #We pretend that this will not happen xD
 M1 <- makeCacheMatrix(M)
 cacheSolve(M1) #inverse returned 
 
 cacheSolve(M1) #inverse returned from cache
 
  





