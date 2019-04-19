## makeCacheMatrix and cachesolve are two functions that when paired, will 
## calculate the inverse of a given matrix and then cache the solution for 
## future calculations

## makeCacheMatrix will not actually produce the inverse, but must be coupled with 
## running cacheSolve. To execute the inverse, makeCacheMatrix will be contained  
## in the argument of cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## we use this part later
        ## This part is where we make sure that the variables can be accessed 
        ## from the parent environment
        set <- function(y) {
                x <<- y ## sets the value and assigns it to x
                m <<- NULL ## clears any data that was stored in m before
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,   ##this part assigns each of the above functions
             setinverse = setinverse, ##to a list within the parent environment
             getinverse = getinverse) ##it also gives names to each of the functions
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        
        m <- x$getinverse() ## here the function calls to see if there was an inverse
        ## calculated for that variable previously
        ## this part is what sets the cache and returns a message if the matrix
        ## inverse is already cached
        if(!is.null(m)) {      ## if m contains data it will return the following
                message("getting cached data")
                return(m) ## and returns the previously cached data
        }
        data <- x$get()
        m <- solve(data, ...) ##here is where the inverse of the matrix is calculated
        x$setinverse(m) ## and then set for the next time it will be asked for
        m
}