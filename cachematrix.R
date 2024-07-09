## Overall, this R script creates a special matrix object that stores the 
## inverse of a matrix as an in-memory object. This may be necessary as
## matrix inversion is usually a cumbersome calculation.  


## The makeCacheMatrix function creates a special R object that stores a matrix
## and its inverse. Using set() the function assigns the input argument to the
## x object, and the value of NULL in the m object in the parent environment
## (aka setter for x and m object). Using get(), the function defines the getter
## for the matrix x. The setinverse and getinverse are the setters and getters
## for the matrix inverse. Using list(), the function assigns each of the above
## function as an element, and returns it to the parent environment. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invertmatrix) inv <<- invertmatrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function populates and retrieves the inverse from an object
## of type makeCacheMatrix. It calls the getinverse() function on the input
## object and then checks to see if the value is NULL. Whenever a new matrix is
## set into the object, there is a valid cached inverse and can return it to the
## parent environment. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ## Return a matrix that is the inverse of 'x'
    if(!is.null(inv)) {
        message("getting inverted matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ....)
    x$setinverse(inv)
}

