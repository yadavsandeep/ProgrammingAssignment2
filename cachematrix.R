## This function
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function (solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Computes the inverse f a matrix by calling makeCacheMatrix if cache already exists

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

