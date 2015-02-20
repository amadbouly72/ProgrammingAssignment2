
## The The first function "makeCacheMatrix" creates a special "Matrix", which is really a list containing a function to
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


## The following function calculates the inverse of the special "Matrix" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the Matrix and sets 
## the value of the inverse matrix in the cache via the setinv() function.

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