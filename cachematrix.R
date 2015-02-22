## Purpose: instead of computing matrix inverse for each iteration, store results in cache and retrieve as needed
## ... computationally less demanding especially when matrices become extremely large since results already stored

## MakeCacheMatrix is first creating an empty matrix for future results to be cached and later retrieved
## Each line is defining a specific function to help define what needs to be calculate
## A list is formed is used to help decrease the ambiguity, e.g. instead of set belonging to element 1, it belongs to 
#... element "set"

#This function, makeCacheMatrix, does the following:
# 1. Set the inverse of the matrix
# 2. Gets the inverse of the matrix
# 3. Sets the inverse of the matrix
# 4. Gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

# CacheSolve Solves the inverse of the cached matrix; if there is no cached matrix, 
# it will cache a new data set into cacheMatrix and start the process to calculate the inverse
# 1. Gets cached inverse matrix
# 2. If cached inverse is not null (i.e already solved), then it returns the inverse matrix that has been cached
# 3. If cached inverse matrix is null, then it uses the makeCachematrix to cache data set into 'inv'
# 4. After cache data into inv, which is NULL, it solves for the inverse of the matrix and produces the output
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Trial run: 
## my_matrix= matrix(c(1, 1, 2, 4, 1, 3, 2, 2, 1), nrow = 3, ncol = 3)
## m = makeCacheMatrix(my_matrix)
## cacheSolve(m)

## > m$get()
## [,1] [,2] [,3]
## [1,]    1    4    2
## [2,]    1    1    2
## [3,]    2    3    1

## > cacheSolve(m)
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333

## > cacheSolve(m)
## getting cached data
## [,1]       [,2]       [,3]
## [1,] -0.5555556  0.2222222  0.6666667
## [2,]  0.3333333 -0.3333333  0.0000000
## [3,]  0.1111111  0.5555556 -0.3333333