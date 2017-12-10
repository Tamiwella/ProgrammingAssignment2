## The two main functions makeCacheMatrix and cacheSolve work together using their shared parent environment to cache the results of and solve an inverse matrix. makeCacheMatrix intialises the x matrix and the m repository that store the answers to the solve function that is run from cacheSolve - this means that any matrix that has already been inverted is excluded from being re-calculated, saving memory for functions and creating more efficient code.

## This fuction creates an empty repository with m that sets the default to NULL. If a matrix passed into the function has not already been calculated, it will remain at NULL. The set function initialises x(intitialsed empty matrix) and m in the parent environment using the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # create empty repository for inverted matrices
    set <- function(y) {
        x <<- y #pointer to x in parent environment / x already intialised as matrix in function
        m <<- NULL #set m to default NULL in parent enviroment
    }
    
    get <- function() x #calls the value of x as it is called outside the function, it is referenced in the parent environment
    setCacheMatrix <- function(result) m <<- result #creating the cache - when x$setCacheMatrix(m) is run in cacheSolve, it stores the result in this anonymous function
    getCacheMatrix <- function() m #retrieves the value of m when cacheSolve is run
    list(set = set, get = get,
    getCacheMatrix = getCacheMatrix,
    setCacheMatrix = setCacheMatrix #this list creates the objects in the parent environment to be used by cacheSolve on run
    )
}

##This function checks that the answer has not already been stored in makeCacheMatrix and if yes, retrieves it, if no, calculates it. makeCacheMatrix must be called in order to call cacheSolve.
cacheSolve <- function(x, ...) {
    mtrx <- x$getCacheMatrix() #sets a variable to the contents of getCacheMatrix in previous function
    if(!is.null(mtrx)) {
        message("getting cached data") #If condition checks if m is NULL for this argument
        return(mtrx)
    }
    mtrx <- x$get() #calls x from previous function
    m <- solve(mtrx, ...) #calculates inverse of matrix
    x$setCacheMatrix(m) #set new value for m in previous function
    m # returns result
}
