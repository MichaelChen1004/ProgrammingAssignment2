## To cache the inverse of a given square matrix, if the inverse of the specific 
## matrix never been calculated before, then it will be calculated and be cached
## in the memory, next time, if the inverse of the same matrix will be needed  
## again, then the function can retrive the cached inverse directly from the 
## memory.

## makeCacheMatrix function is to create a function closure of which the main 
## purpose is to hold the inverse of a specific given matrix as well as providing
## other subfunctions such as set the matrix, retrive the matrix and retrive the
## the preserved inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL   ## this variable will hold the inverse
    setMatrix <- function(y) x <<- y  ## reset the matrix which is used to calculate
                                        ## the inverse
    getMatrix <- function() x  ## retrive the matrix which was given.
    setInverse <- function(ivse) m <<- ivse ## cache the inverse
    getInverse <- function() m  ## retrive the cached inverse
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)  ## create a function list of all the functions
                                    ## in the enviroment.
}


## cacheSolve is to calcualte the inverse of the given matrix if it has not been
## calculated before, and cache the result in memory. Next time, if encounter the
## same matrix calculated before, the inverse will be retrived directly from cache.

cacheSolve <- function(x, ...) {
    
    if (!is.null(x$getInverse())) {   ## if can find the cached inverse
        print("Retrived from cache!")
        x$getInverse()
    } else {    ## is no cached inverse
        m <- solve(x$getMatrix())
        x$setInverse(m)
        m
    }
    ## Return a matrix that is the inverse of 'x'
}
