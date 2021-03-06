## x: a square invertible matrix
## return: a list containing functions to
##      a. set the matrix
##      b. get the matrix
##      c. set the inverse
##      d. get the inverse
## this list is used as the input to cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
        ## return: a list containing functions to
        ##      a. set the matrix
        ##      b. get the matrix
        ##      c. set the inverse
        ##      d. get the inverse        
        i = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inverse) i <<- inverse 
        getinv = function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## x: output of makeCacheMatrix function
        ## return: inverse of the original matrix input    
        
        i = x$getinv()
        
        ## if the inverse is calculated
        if (!is.null(i)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(i)
        }
        
        ## if the inverse is not calculated, calculates the inverse
        mat.data = x$get()
        i = solve(mat.data, ...)
        x$setinv(i)
        return(i)
}

