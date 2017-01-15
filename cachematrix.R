## Two Functions use to create matrix object and to Cache the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set function to set the square invertible matrix
    set <- function(y)  
    {
        x <<- y
        inv <<- NULL
    }
    ## get function to get the square invertible matrix
    get <- function() x 
    
    ## setInverse function to set the inverse of square invertible matrix
    setInverse <- function(inverse) inv <<- inverse 
   
    ## getInverse function to get inverse of square invertible matrix
    getInverse <- function() inv  
            
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          
    inv <- x$getInverse()
    if (!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)  ## Return a matrix that is the inverse of 'x', retrieve from cache
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)  ## Computing the inverse of a square invertible  matrix
    x$setInverse(inv)
    inv  ## Return a computed matrix that is the inverse of 'x'

}
