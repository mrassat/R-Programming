## The objective of these two functions is to "cache" matrix inverse operations
## to avoid reapeating this costly computation operation

require(MASS)

## It creates a special "matrix" object that can catch its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv_mat <<- inverse
        getInverse <- function() inv_mat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## It computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has been calculated previously, it returns that matrix
## instead of calculating it again.
cacheSolve <- function(x, ...) {
        inv_mat <- x$getInverse()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        m <- x$get()
        ##inv_mat <- solve(m, ...)
        inv_mat <- ginv(m)
        x$setInverse(inv_mat)

        ## Return a matrix that is the inverse of 'x'
        inv_mat
}
