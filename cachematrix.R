# A pair of functions caching the Inverse of a Matrix

# Creates a special matrix object caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Method to get the matrix
        get <- function() {
                x
        }
        
        # Method to set the inverse of the matrix
        setInverse <- function(inverse){
                inv <<- inverse
        }
        
        # Method to get the inverse of the matrix
        getInverse <- function() {
                inv
        }
        
        # Return a list of the above methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Compute the inverse of the special matrix created by "makeCacheMatrix"
# If the inverse has been calculated and the matrix does not change, then
# the "cacheSolve" would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # Return the inverse if exists
        if( !is.null(m) ){
                message( "getting cached data" )
                return(m)
        }
        
        # otherwise, get the matrix from the object
        data <- x$get()
        # calculate the inverse
        m <- solve(data)
        # set the inverse to the object
        x$setInverse(m)
        # return the matrix
        m
}
