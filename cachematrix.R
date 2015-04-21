
## The first function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix through solve()
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # stores the cached value
        # initialize to NULL
        inv <- NULL
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # invert the matrix and store in cache
        set_inverse <- function(solve) inv <<- solve
        # get the inverted matrix from cache
        get_inverse <- function() inv
        # return the created functions to the working environment
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if the 
##inverse has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse 
##of the matrix and sets inverse in the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## attempt to get the inverse of the matrix stored in cache
        inv <- x$get_inverse()
        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                inv <- solve(data, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$set_inverse(inv)
        } )
        
        #display matrix in console
        return(inv)
}


