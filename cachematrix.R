
##'makeCacheMatrix' is a function creates a special "matrix" object that can 
##'cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ##defines the argument of the matrix. 
        inv <- NULL  ##define inv as NULL; this will keep the value of the 
        ##inverse matrix.
        set <- function(y) {  ##define the set function to assign a new value of 
                x <<- y       ##the matrix in the parent environment if there is
                inv <<- NULL  ## a new matrix, reset inv to NULL.
        }
        get <- function() x   ##define the get function  - returns the value of
        ##the array argument
        setInverse <- function(inverse) inv <<- inverse ##assigns value of inv
        ##in parent environment.
        getInverse <- function() inv   ##gets the value of inv where called.
        list(set = set, get = get,     ##list of instructions used to refer 
             setInverse = setInverse,  ##to functions with the operator '$'
             getInverse = getInverse)
}


##'cacheSolve' is a function computes the inverse of the special "matrix" 
##returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {  ##defines the argument of the matrix.
        inv <- x$getInverse()     ##returns the inverse if it is already 
        ##calculated.
        if(!is.null(inv)) {       
                message("getting cached data")
                return(inv)
        }
        data <- x$get()          ##obtain the matrix.
        inv <- solve(data, ...)  ##calculate the inverse.
        x$setInverse(inv)        ## establishes the inverse.
        inv                      ##returns the matrix.
}


##First define the parameters of your Matrix

m1 <- makeCacheMatrix(c(1, 2, 3, 4))
m1 <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

##Apply the functions that were created.

m1$get()
##[,1]     [,2]
##[1,]    1    3
##[2,]    2    4

m1$getInverse()
##NULL

cacheSolve(m1)
##[,1]     [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve(m1)
##getting cached data
##[,1]     [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

m1$getInverse()
##[,1]     [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



##Functions:

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL  
        set <- function(y) {   
                x <<- y       
                inv <<- NULL  
        }
        get <- function() x   
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv   
        list(set = set, get = get,      
             setInverse = setInverse,  
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) { 
        inv <- x$getInverse()     
        if(!is.null(inv)) {       
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         
        inv <- solve(data, ...)  
        x$setInverse(inv)        
        inv                      
}
