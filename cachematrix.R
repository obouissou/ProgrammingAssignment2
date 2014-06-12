## 2 functions are provided
##  makeCacheMatrix:  
##     Creates an object of type CacheMatrix
##     CacheMatrix is an object that store a Matrix and automatically store its inverse the first time the inverse of the matrix is calculated
##  cacheSolve: 
##     Returns the inverse of a matrix stored in a CacheMatrix object.
##     The first time cacheSolve is called on a CacheMatrix CM the inverse is calculated and stored into CM  
##     THe next times cacheSolve is called on CM, the inverse is retrieved form, the cache, and not calculated.
##
##  The code is adapted from makeVector and cachemean given as example in the assignment
##

##
## Creates an object of type CacheMatrix which is a list of function for storing/retrieving a matrix and its inverse 
##
makeCacheMatrix <- function(x = matrix()) {

        # set default value  to NULL for the cached inverse when the cache matrix x is first created
        inv <- NULL

        # The set function is called to change the value of the matrix stored in the CM
        # Since the value of the matrix is changed the inverse value must be reinitialized to NULL
        # The new value has to be stored in x (in case get function is called afterwards). Since x and inv are located in the parent level of 
        #  where set is defined they are assigned with "<<-" instead of "<-"            
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # return the matrix "as is" 
        get <- function() x

        # store the inverse into the inv variable
        # same as above "<<-" to assign a value to a variable outside the setinv function
        setinv <- function(z) inv <<- z
        
        # return the value as cached
        getinv <- function() {        
           inv
        }

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



##
## cacheSolve returns the inverse of CacheMatrix given in parameter
##   If the inverse is stored in the cache then the value in the n cache is returned
##   IF the inverse is not stored in the cache then the inverse is computed and stored in the cache
##

cacheSolve <- function(x, ...) {
        # check if inverse of matrice is stored in cache?
        # if yes return value fetched from cache 
        if(!is.null(x$getinv())) {
          message("Fetch cached value")
          return(x$getinv())
        }

        # if not stored in cache
        # 1) compute inverse
        inv <- solve(x$get())
        # 2) store in cache
        x$setinv(inv)
        # 3) return computed value
        return(inv)
}
