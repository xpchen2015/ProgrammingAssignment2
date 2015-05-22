## The two functions are used to crate a special object that stores a matrix 
##and cache's its inverse.
## If global and permanent assignments are intended within a function, 
##then either the "superassignment" operator, <<- or 
##the function assign() can be used. 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        IM<- NULL
        setM <- function (y){
                x<<-y
                IM<<-NULL
        }
        getM <- function() x
        setIM <- function(INV) IM <<- INV
        getIM <- function() IM
        list(setM = setM, getM=getM,
             setIM = setIM, getIM = getIM)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
#If the inverse has already been calculated, 
##then the cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        IM <- x$getIM()
        if(!is.null(IM)){
                message ("getting cached matrix")
                return(IM)
        }
        data <- x$getM()
        IM <- solve(data)
        x$setIM(IM)
        IM
}
