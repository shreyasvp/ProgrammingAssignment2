## Date: 23rd Jan 2015
## R Programming Assignment 2: Lexical Scoping 


## makeCacheMatrix:

## makeCacheMatrix creates a special matrix object that can cache the inverse of a matrix. Inverse is calculated
## by using the Solve function in R. All matrices supplied are square invertible matrices.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL                             # Initializes the Inverse storage variable to NULL
       
        set <- function(y) {                  # Set function caches dataset/matrix y to the current working object x
            x <<- y                           # Caches Null to m since the matrix has been changed.
            m <<- NULL
        }
        get <- function() x                   # returns the given matrix x
        setInverse <- function(Inv) m <<- Inv # Caches the current value of the inverse in m
        getInverse <- function() m            # returns the current value of m
        list(set = set, get = get,
            setInverse = setInverse, 
            getInverse = getInverse)
    
}


## cacheSolve:

## cacheSolve takes a matrix and returns its inverse. 
## It checks the cache to see if the Inverse has already been calculated for that matrix and pulls it from the cache.
## instead of recalculating it. The Inverse retrieval is only valid if the matrix has not been changed before running
## the function.

cacheSolve <- function(y, ...) {
    
        inver <- y$getInverse() 
                                       # Checks data received by getInverse to determine 
                                       # if inverse has been cached or not
        
        if (!is.null(inver)){
            print(inver)
            message("Getting Cached Data")
            return(inver)              # Returns cached data.
        }
                                       # Calculates Inverse of matrix and Caches it.
        
        data <- y$get()                # obtains matrix to be inverted
        inver <- solve(data, ...)      # Actual matrix inversion
        y$setInverse(inver)            # Caching of inverse
        inver                          # Return a matrix that is the inverse of 'x'
}
