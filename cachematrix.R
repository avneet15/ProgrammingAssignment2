## Creating 2 functions which can cache the inverse of a matrix


## This function creates a matrix which can be cached, internally creating 4 functions i.e 
## a)get b)set c)getinverse d)setinverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
			x <<- y
			inverse <<- NULL
		   }
    get <- function() x
    getinverse <- function() inverse
    setinverse <- function(inv) {
					inverse <<- inv
				}
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)				

}


## This function takes a cacheable matrix as input and return the inverse either from the cache or computes it and assigns it to the matrix object.

cacheSolve <- function(x, ...) {
            inverse <- x$getinverse()
			if(!is.null(inverse)) {
				message("getting cached data")
                return(inverse)
			}
			data <- x$get()
			inverse <- solve(data)
			x$setinverse(inverse)
			inverse
        ## Return a matrix that is the inverse of 'x'
}
