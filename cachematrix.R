## Below are a pair of functions that cache the inverse of a matrix. Matrix 
## inversion is a costly computation and there may be benefits to cache the 
## inverse of a matrix rather than compute it repeatedly. This benefit may be  
## realised if the contents of the matrix are not changing.    

## makeCacheMatrix Function: This function creates a specal "matrix" that can 
##   cache its inverse. For this assignment, assume that matrix supplied is 
##   always invertible. 
makeCacheMatrix <- function(x = matrix()) {
        
        ## initialise the inverse matrix variable
        s <- NULL
        
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
                
        }
        
        ## get the value of the matrix
        get <- function () x
        
        ## set the value of the inverse matrix
        setsolve <- function(solve) s <<- solve 
        
        ## get the value of the inverse matrix
        getsolve <- function() s
        
        ## lists the defined functions 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve Function: This function calculates the inverse matrix created  
##   using above function. It checks to see if the inverse matrix has already
##   been calculated.if so, it gets the inverse matrix from the cache and skips
##   the computation. Otherwise, it calculates the inverse matrix of the data   
##   and set the value of the inverse matrix in the cache via the setsolve 
##   function.  
cacheSolve <- function(x, ...) {
        
        ## get the value of the cached inverse matrix
        s <- x$getsolve()
        
        ## check whether there was a value returned from cached 
        ## i.e. if value of inverse matrix was cached
        if(!is.null(s)){
                message("getting cached data")
                return (s)
        }
        
        ## get value of matrix
        data <- x$get()
        
        ## get value of inverse matrix using solve function from library 
        s <- solve(data, ...)
        
        ## set the value of the inverse matrix in the cache
        x$setsolve(s)
        
        ## Return a matrix that is the inverse of 'x'
        return(s)
}

## TESTING INSTRUCTIONS:
## 1. Create a new invertible matrix, e.g.
##        m <- matrix(c(1,-1/4,1/4,1),2,2)
## 2. Apply following line to test code
##        m1 < makeCacheMatrix(m)
## 3. Test the cacheSolve function to get desired result
##        cacheSolve(m1)