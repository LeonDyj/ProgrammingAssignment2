##Matrix inversion is usually a costly computation 
##and their may be some benefit to caching the 
##inverse of a matrix rather than compute it 
##repeatedly (there are also alternatives to 
##matrix inversion that we will not discuss here). 
##This assignment is to write a pair of functions 
##that cache the inverse of a matrix.

## This function is to generate four functions which are
## set(),get(),setinv() and getinv()

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        set <- function(y){
                x <<- y
                mat <<- NULL
        }
        get <-function()x
        setinv <-function(minv) mat <<-minv
        getinv <-function() mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function is to calc the inverse of a matrix,
## if the inverse of the giving matrix has been calced
## earilier, the function will read the memory directily.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinv()
        if(!is.null(mat_inv)){
                message("getting cached data")
                print('hello')
                return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data,...)
        x$setinv(mat_inv)
        mat_inv
}
