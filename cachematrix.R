# In this script we introduce the <<- operator which can be used to assign a value to an object 
# in an environment that is different from the current environment. Below are two functions that 
# are used to create a special object that stores an invertible matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
     

     m <- NULL      
     
     set <- function(y) {
    
          
          x <<- y                  # set the invertible matrix x to a new matrix y
          m <<- NULL               # reset the inverse m to NULL.  m == Null is interpeted as 
                                   # "the inverse value of invertible matrix x is NOT in cache"                                   
     }
     get <- function() x                                    # returns the invertible matrix x
     setinverse <- function(inverse) m <<- inverse          # sets the inverse m to inverse
     getinverse <- function() m                             # returns the inverse m
     
     # return the list containing all of the functions defined above
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
     
     # retrieve the value of the inverse (m), which may possibly have been previously cached  
     m <- x$getinverse()
    
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     message("No cached data => Calulate inverse")     # the inverse does not exist in cache
     data <- x$get()                                   # retrieve the invertible matrix encapsulated 
                                                       # in object variable x
     m <- solve(data, ...)                             # calculate the inverse of the retrieved data
     x$setinverse(m)                                   # cache the inverse matrix
     m                                                 # return the inverse matrix
}
