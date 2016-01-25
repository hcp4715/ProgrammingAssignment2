## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
