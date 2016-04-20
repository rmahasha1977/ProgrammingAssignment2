## There are two functions defined in this R File.
## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The second function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. This is because calculating the inverse of a
## matrix might be a costly operation.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse of the matrix to NULL
        inv <- NULL
        
        ## Now create the functions 'set', 'get', 'setInverse' and 'getInverse' and add to the list
        ## <<- operator is used to assign a value to an object in an environment that is different 
        ## from the current environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First try to get the inverse from the list. If this inverse is NULL then calculate
        ## the inverse and cache it. Otherwise just return what we get from the "getInverse"
        ## function
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
