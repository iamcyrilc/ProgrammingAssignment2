## R Programming assignment to understand environment and lexical scoping
## This application test caching and retrieving inverse of a matrix .

## This method is used to set matrix, get matrix, retrive cached inverse matrix and cache inverse matrix
## set -> reset a new value for seed matrix and initialize the cache
## get -> return the original matrix
## getInverse -> return the cached inverse matrix
## setInverse -> cache the inverse matrix
## output of the function is list of above four methods.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # reset cached inverse matrix and use a different matrix
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        
        # get the matrix from the environment
        get <- function() x
        
        # cache the inverse matrix
        setInverse <- function (inverse)
        {
                print("Inverse matrix cached.")
                inv <<- inverse
        }
        
        # Get the cached inverse matrix
        getInverse <- function() inv
        
        list (set = set,get = get,setInverse = setInverse, getInverse = getInverse)
}


## This method returns inverse of a matrix.
## The cache is first checked to see if inverse matrix exist.
## If inverse matrix exist in the cache, it is returned, if not inverse matrix is
## created and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv))
        {
                print("Inverse matrix found in cache.")
                return (inv)
        }
        
        print("Inverse matrix not found in cache, caching inverse matrix.")
        
        #get data, inverse and cache
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        
        inv
}
