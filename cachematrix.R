## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {##Set the values of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## Get the values of the matrix
        setInverse <- function(inverse) inv <<- inverse  ##Set the values of the matrix inverse
        getInverse <- function() inv ##Get the values of the matrix inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {  ##checks if the inverse matrix has already been calculated 
                message("getting cached data")
                return(inv) ##If yes returns the value calculated in the previous function, skiping the computation.
        }
        mat <- x$get() 
        inv <- solve(mat, ...) ##If no gets the data from the matrix and calculates its inverse
        x$setInverse(inv)
        inv
}


## TEST RESULT : 

##> source("C:/R-3.3.0/workspace/cachematrix.R")

##set the value of the metrix 
##> matrix1 <- makeCacheMatrix(matrix(c(1,2,2,1), 2, 2))

##get the value of the metrix
##> matrix1$get()
##     [,1] [,2]
##[1,]    1    2
##[2,]    2    1

##set the value of the inverse // no cache (first time run)
##> cacheSolve(matrix1)
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

##get the value of the inverse
##> cacheSolve(matrix1)
##getting cached data
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> 

