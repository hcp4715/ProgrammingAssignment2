## This R script include two funcitons, one (makeCacheMatrix) is to create a speical matrix that
## can cache its inverse, the other is to compute the inverse of that speical 
## matrix and return the cached inverse, instead of compute the inverse repeatedly.

## DATE: 01/28/2018
## Authoer: Chuan-Peng Hu


## This function is for creating a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setReverse <- function(solve) m <<- solve
        getReverse <- function() m
        list(set = set,get = get,
             setReverse = setReverse,
             getReverse = getReverse)

}


## This function compute the inverse of the special matrix
##  and returned by "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getReverse()
        if(!is.null(m)){
                message(" getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setReverse(m)
        m
}

