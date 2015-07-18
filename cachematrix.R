## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeVector creates a special "vector", a list containing a function to
## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the value of the inverse matrix
## 4)get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## 1)set the value of the matrix
    set <- function(y){
        ## search through the parent env for existing definition of the variable
        x <<- y
        inv <<- NULL
    }
    
    ## 2)get the value of the matrix
    get <- function(){x}
    
    ## 3)set the value of the inverse matrix
    setInverse <- function(x){inv <<- x}
    
    ## 4)get the value of the inverse matrix
    getInverse <- function() {inv}
    
    ## return the list of functions
    return (list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


## Write a short comment describing this function
## Cache the inversed matrix if it has been computed already. 
## Otherwiese computet the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    ## when it is null, compute the inverse
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    return (inv)
}
