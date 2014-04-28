## Coursera - R Programming (rprog-002)
## Programming Assignment 2
##
## Author: Frederico Munoz <fsmunoz@gmail.com>
##
## Put comments here that give an overall description of what your
## functions do

##  This function takes as argument a matrix and creates a set of
##  functions which are stored in a list and returned; this functions
##  make use of scoping rules to encapsulate values and are callable -
##  the result is a sort of "object" in a way, that contains both data
##  and functions.
##
##  More explanations are in-lined.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialise the inverted value to NULL
    m <- NULL
    ## Create function that returns the passed matrix
    get <- function() x    
    ## Create function to set the the input matrix Note that "foo" is
    ## a free variable, and that both assignements are done using
    ## "<<-" to "reach" the variables in the surrounding environment
    ## (which is the makeCacheMatrix body itself).
    set <- function(foo) {
        x <<- foo
        m <<- NULL 
    }
    ## Set the inverse (via superassignment)
    setinverse <- function(inverse) m <<- inverse     
    ## Returns inverse
    getinverse <- function() m
    ## Packages functions in a list which is then returned
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Takes the "object" (a list, in fact) created by makeCacheMatrix and
## uses the functions it provides to check for cached data (and return
## it) or, should it not exist, calculates it and stores it.

cacheSolve <- function(x, ...) {
    ## Use the getinverse function in the list of fuctions created before
    m <- x$getinverse()
    ## Check if it exists: is it does then use the values
    if(!is.null(m)) {
        message("Accessing cached data...")
        return(m)
    }
    ##... if not then calculate it and store it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m 
}
