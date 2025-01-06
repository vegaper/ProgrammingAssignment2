## this creates the functions to interact with the object
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) {I <<- inv}
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## this calculates the inverse of the matrix and stores it using setInverse

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}

#example use> 
# matrix_A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
# matrix_B <- matrix(c(2, 2, 3, 4), nrow = 2, ncol = 2)
# m_A <- makeCacheMatrix(matrix_A)
# m_B <- makeCacheMatrix(matrix_B)
# cacheSolve(m_A)
# cacheSolve(m_A)
# cacheSolve(m_B)
