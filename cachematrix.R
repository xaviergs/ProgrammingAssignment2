## This set of functions perform the next tasks:
## makeCacheMatrix: Creates a "matrix-object" that gets and sets
##      a object  and provides two methods for getting and setting
##      its inverse. Uses the <<- operator to cache the object in 
##      the parent environment, so it will persist and can be queried
##      from different calls to the matrix-object
## cacheSolve: Uses the makeCacheMatrix object to evaluate the inverse
##      of a matrix. Has the hability of querying the cache to know
##      if the inverse object alredy exists and avoid time consuming 
##      calcualtions. Also, it provides an starting check of feasibility
##      of the inverse matrix operation by verifying its dimensions

##############################################################
##      makeCacheMatrix -> creates a matrix-object composed by data and
##      methods to query it
makeCacheMatrix <- function(x = matrix()){
        ##      Initializes the cached working matrix
        cachedMatrix <- NULL
        ## Internal functions
        ## set: stores the passed matrix
        set <- function(y){
                x <<- y
                cachedMatrix <<- NULL
        }
        ## get: returns the working matrix
        get <- function() x
        ## setInverse: evaluates the inverse of the
        ##      working matrix using the "solve" R function
        setInverse <- function(solve) cachedMatrix <<- solve
        ## getInverse: returns the matrix inverse
        getInverse <- function() cachedMatrix
        
        ## Returns a list containing the four useful functions
        ##      for the cached matrix object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##############################################################
##      cacheSolve -> caches the inverse of a matrix-object using
##      both the internal methods and the hability of accessing
##      the parent environment
cacheSolve <- function(x,...){
        ##      Gets the value for the inverse in the working structure
        invMatrix <- x$getInverse()
        ##      When cached inverse exists, shows the message
        ##      and returns the cached value
        if(!is.null(invMatrix)){
                message("Info.: Getting cached data")
                return(invMatrix)
        }
        
        ##      Gets the working structure
        workMatrix <- x$get()
        ##      Verifies that the working matrix is an square one, 
        ##      otherwise the inverse cannot be
        ##      calculated
        if(nrow(workMatrix) != ncol(workMatrix)){
                message("Error: Only squared matrices can be inverted.")
                message(paste("Can't perform the calculation, the argument passed is a (",
                              toString(nrow(workMatrix))," row X ", 
                              toString(ncol(workMatrix))," col) matrix",
                              sep =""))
                return(workMatrix)
        }
        ##      Shows a warning informing that the operation
        ##      requested can take some time
        message("Warning: Calculating inverse, it can take a while...")
        tStart <- Sys.time()
        ##      Applies the structure function to get the inverse
        invMatrix <- solve(workMatrix, ...)
        tEnd <- Sys.time()
        ##      Shows an info message with the time taken to perform the calcualtion.
        message(paste("Info: Inverse matrix calculation took:",
                      toString(round(tEnd - tStart,3)),
                      " sec.",
                      sep = ""))
        ##      Sets the inverse into the structure's cache
        x$setInverse(invMatrix)
        ##      returns the inverse
        invMatrix
}


