## Support inverting a matrix and caching the inverted form. It is expected that caching an 
## inverted matrix is computationally less expensive than calculating the inverse each time. 
##
## The first function 'makeCacheMatrix' wraps the input matrix 'x' such that it's inverted 
## value can be subsequently cached.
##
## The second function 'cacheSolve' will return the cached inverted matrix if it has
## already been calculated. Otherwise, it inverts the matrix, caches it, and returns the
## inverted form.


## makeCacheMatrix: Builds a wrapper around a matrix to cache its inverted value
##
##  Parameters:
##      'x' is a matrix we want to cache for subsequent inversion
##
##  Returns:
##      a wrapper around a matrix that will cache its inversion
##    
##  History:
##      2014/June/14 - Original version by Dwight Kruger
makeCacheMatrix <- function(x = matrix()) {
    
    invertedMatrix <- NULL
    
    # Specify the set() function for cacheMatrix
    set <- function(y) {
        x <<- y                   # Save the value in the parent environment
        invertedMatrix <<- NULL   # Save the value in the parent environment
    }
    
    # Specify the get() function for cacheMatrix
    get <- function() x
    
    # Specify the setInverse function
    setInverse <- function(inverse) invertedMatrix <<- inverse
    
    # Specify the getInverse function
    getInverse <- function() invertedMatrix
    
    # Set the methods on this object
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##  cacheSolve: Returns a matrix that is the inverse of 'x'. If the matrix has already been
##  inverted, use the cache value.
##
##  Parameters:
##      'x' is a matrix that has been constructed with makeCacheMatrix
##
##  Returns:
##      the inverted version of the matrix in x
##    
##  History:
##      2014/June/14 - Original version by Dwight Kruger
cacheSolve <- function(x, ...) {
    
    # Try to get the inverted value of the matrix from the cache. If it exists, return it here.
    inverseMatrix <- x$getInverse()
    if (!is.null(inverseMatrix)) {
        message("getting cache state")
        return(inverseMatrix)
    }
    
    # We have not inverted this matrix as yet. Invert it, and cache the inverted value
    data <- x$get(...)
    
    inverseMatrix <- solve(data)
    x$setInverse(inverseMatrix)
    
    # Return the inverted matrix
    inverseMatrix
}
