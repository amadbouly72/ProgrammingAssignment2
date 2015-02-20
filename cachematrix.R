
## In The following I wrote a pair of functions that compute and cache the inverse of a given matrix.

## The first function "makeCacheMatrix" creates a special "Matrix" object, that can cache its inverse, it is really a list containing a function to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. set the value of the Inverse Matrix
## 4. get the value of the Inverse Matrix  

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function (y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mat <<- solve
        getinv <- function() mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function "cacheSolve" calculates the inverse of the special "Matrix" 
## object created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation, this is indicated by the "getting cached inverse matrix data"
## sentence shown before displaying the inverse matrix result . Otherwise, it calculates the 
## inverse of the Matrix and sets the value of the inverse matrix in the cache via the setinv() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting cached inverse matrix data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinv(mat)
        mat
}