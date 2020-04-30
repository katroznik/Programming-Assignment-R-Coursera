## The functions below cache the inverse of a matrix. 

## makeCacheMatrix function creates matrix object that can chace its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by the code above (makeCacheMatrix).

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting an inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}


#Control test
test <- makeCacheMatrix(matrix(c(8,8,12,2), 2, 2))
test$get()
test$getInverse()
cacheSolve(test)
