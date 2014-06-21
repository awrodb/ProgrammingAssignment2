## This function manages the caching of given matrix (set() FUN) and inverse matrix (setinvmx() FUN). 
## Moreover, the same function recalls the original matrix from cache (get() FUN) and the already 
## calculated inverse maatrix (getinvmx() FUN).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmx <- function(solve) m <<- solve ## caching inverse matrix
        getinvmx <- function() m
        list(set = set, get = get,
             setinvmx = setinvmx,
             getinvmx = getinvmx)
}


## This function re-calls the previous solved inverse matrix from the cache.
## If the cache is empty the function solves the inverse matrix and store it in the cache

cacheSolve <- function(x, ...) {
        m <- x$getinvmx() ## Re-calling from cache the stored inverse matrix or NULL
        ## If the inverse matrix is calculated the function recalls the stored data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the inverse matrix is not calculated the function solve the inverse 
        ## matrix and stores that in the cache
        else{ 
                message("not in the cache.")
                message("now is calculated and cached")
        }
        data <- x$get()
        m <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
        x$setinvmx(m)
        m
}
