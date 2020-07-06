## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (x = matrix()){ ## first the makeCacheMatrix function is created in which it is declared that x is an matrix
    inv <- NULL                             ## inv initializes it has no values
    set <- function(y){
        x <<- y                             ##matrix value is set
        inv <<- NULL
    }
    get <- function() {x}                   ##we get the value of the matrix
    setInverse <- function(inverseMatrix) {inv <<- inverseMatrix} ##we define the inverse of the matrix
    getInverse <- function() {inv}                                ##we get the inverse of the matrix    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##This function will search inside the cache memory if the array has been stored or not.
    inv <- x$getInverse()        ## returns the inverse of the matrix and assigns it to x
    if(!is.null(inv)) {
        message("getting cached data") ##these lines verify whether or not the matrix information is cached and omit the calculation and print "getting catched data" if yes
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
