# In this script we introduce the <<- operator which can be used to assign a value to an object 
# in an environment that is different from the current environment. Below are two functions that 
# are used to create a special object that stores an invertible matrix and cache's its inverse.

# The makeCacheMatrix function takes as its input an invertible matrix x and creates a special 
# "vector",  which is really a list containing functions to: set the value of the matrix x,
# get the value of the matrix x, set the value of the inverse of the matrix x, get the value 
# of the inverse of the matrix x. To use the stored functions, they need to be extracted from 
# makeCacheMatrix. This is done by assigning makeCacheMatrix to an object variable then using  
# the name of the object + "$" + the name of the function + (arguments)
makeCacheMatrix <- function(x = matrix()) {
     
     # set the inverse to NULL as a placeholder for a future value.  m == Null is interpeted as
     # "the inverse of invertible matrix x is NOT in cache"
     m <- NULL      
     
     set <- function(y) {
          
          # The <<- operator is normally only used in functions, and causes a search to be
          # made through parent environments for an existing definition of the variable 
          # being assigned. If such a variable is found (and its binding is not locked) then 
          # its value is redefined, otherwise assignment takes place in the global environment.       
          
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

# The cacheSolve function takes as its input the special "vector" created with the makeCacheMatrix 
# function above.  NOTE THAT THE INPUT x HERE REFERS TO THE OBJECT VARIABLE TO WHICH makeCacheMatrix
# WAS ASSIGNED.  cacheSolve calculates the inverse of the invertible matrix that was encapsulated by  
# makeCacheMatrix however, it first checks to see if the inverse has already been calculated.  If so,
# it gets the inverse from the cache and skips the computation. Otherwise, it calculates
# the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
     
     # retrieve the value of the inverse (m), which may possibly have been previously cached  
     m <- x$getinverse()
     
     # m == Null is interpeted as "the inverse of the invertible matrix encapsulated in object 
     # variable x is NOT in cache".  If the inverse exists in cache (!is.null(m)), it simply returns 
     # a message and the inverse (m).     
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
