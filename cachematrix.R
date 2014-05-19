## The overall objective of this program is to write a pair of functions 
## that cache the inverse of a matrix. As matrix inversion is a costly computation
## caching will reduce computing it repeatedly.


## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function (inv) m <<- inv
        getinv <- function () m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function(cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
       
}




