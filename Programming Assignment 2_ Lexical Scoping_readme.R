## Programming Assignment 2: Lexical Scoping

# The first function creates a special "vector" and assigned as makeVector.
    # 1. set the value of the vector
    # 2. get the value of the vector
    # 3. set the value of the mean
    # 4. get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}



## The second function calculates the mean of the special "vector" created with the above function.
    # It first checks to see if the mean has alredy been calculated. If so, it gets the mean from the cache and 
    # skils the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the
    # cache via the setmean function.


cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


### Write a pair of functions that cache the inverse of a matrix.

    # 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
    # 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
        # above. If the inverse has already been calculated (and the matrix has not changed), then the
        # cachesolve should retrieve the inverse from the cache.


# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) m<<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix remains unchanged) then the cacheSolve should
# return the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinverse(m)
    m
}
