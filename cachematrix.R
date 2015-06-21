## The functions below stores a matrix and computes its inverse.
## If the inverse has already been cached, then the inverse will be
## stored for further calling.


## Function makeCacheMatrix stores an input matrix and caches 
## the inverse of a matrix that is set by input.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, 
             get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## cacheSolve first checks to see if the inverse matrix 
## already exists in the makeCacheMatrix. If yes, then it returns the
## inverse matrix as set by $set; if not, then it computes the inverse
## matrix of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
