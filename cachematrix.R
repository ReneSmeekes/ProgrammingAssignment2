## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Creates a special matrix with a cachable inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Set the inverse value to missing on creation
        set <- function(y) { ## Sets the matrix and removes the cached inverse
                x <<- y
                i <<- NULL 
        }
        get <- function() x ## Returns the matrix
        setinv <- function(inv) i <<- inv ## Sets the cached inverse manually
        getinv <- function() i ## Returns the cached inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Returns the cached inverse or calculates it when it's not yet cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) { ## Returns the cached inverse if it's available
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...) ## Invert the matrix
        x$setinv(i) ## Set the inverse to the calculated inverse
        i} 
