## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  finv <- NULL
        set <- function(y) {
                x <<- y
                finv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) finv <<- inverse
        getinverse <- function() finv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix created
## with the above function. It first checks to see if the inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  finv <- x$getinverse()
        if(!is.null(finv)) {
                message("getting cached data")
                return(finv)
        }
        data <- x$get()
        finv <- solve(data)
        x$setinverse(finv)
        finv
}