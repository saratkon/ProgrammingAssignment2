## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. We code 
## two functions here that help us in achieving in this cache based inverse operation.
##
## Typical usage of these functions is as follows:
## 
## # Create a matrix 
## x <- matrix(...)
##
## # create a special matrix object
## y <- makeCacheMatrix(x)
##
## # first call computes the inverse
## yInv <- cacheSolve(y)
##
## # any further calls, will fetch from the cache
## yInv <- cacheSolve(y)


## This function creates a special "matrix" object that can cache its inverse.
## This is a list of 4 functions, set, get, setInverse and getInverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse cache to NULL
        inv <- NULL
        
        ## Definition of the set accessor mentod that can be used to set a input matrix
        set <- function(y){
                
                ## Initialize the internal matric object with that of the input
                x <<- y
                
                ## Since a new matrix is set, reset the cache if already calculated
                inv <<- NULL
        }
        
        ## Definition of the get accessor mentod
        get <- function() x
        
        ## Definition of the SetInverse accessor mentod
        setInverse <- function(inverse) inv <<- inverse
        
        ## Definition of the getInverse accessor mentod
        getInverse <- function() inv
        
        ## The function returns a list of the four methods defined above
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This is the main function that computes the matrix inverse
cacheSolve <- function(x, ...) {
        
        ## Fetch the cached matrix inverse
        invX <- x$getInverse()
        
        ## Check if the cached value is valid, indicating if the inverse has already been calculated
        if(!is.null(invX)){
                ## if the cache is not NULL, then print a message indicating the return of the cached value
                message("getting cached data")
                
                ## return the cached value
                return (invX)
        }
        
        ## The cached value is NULL and hence we perform the actual inverse calculation here
        
        ## Get the matrix object
        data <- x$get()
        
        ## Calculate the inverse using the solve method.
        invX <- solve(data, ...)
        
        ## Set the cache
        x$setInverse(invX)
        
        ## return the calculated inverse
        invX
}
