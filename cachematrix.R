## The following code defines two functions:
## makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special list, which has four functions, to view
## and set the value of a matrix and its inverse, using lexical scoping
## cacheSolve looks at the inverse of the matrix in cache. If it exists, cacheSolve
## gives cached inverse as result, otherwise it computes the inverse of matrix, and
## stores it in cache


## makeCacheMatrix creates a "matrix", which essentially is the list containing four 
## functions:
## 1) set() : It sets the value of matrix, and erases previously stored inverse, if any
## 2) get() : It returns the matrix created 
## 3) setinv(): It takes the inverse of the matrix as argument, and sets the inverse
## 4) getinv(): It returns the inverse of the matrix
##
## The function makeCacheMatrix takes a matrix as argument. If no argument is 
## specified, it then creates an empty matrix by default.
##
## The function returns a list containing the above mentioned four functions.


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function (y){
        x <<- y
        inv <<- NULL        
    }
    
    get <- function() x
    
    setinv <- function(i){
        inv <<- i
    }
    
    getinv <- function() inv
    
    list ( set = set, 
           get = get, 
           setinv = setinv, 
           getinv = getinv )
    
}


## cacheSolve is a function, that takes the special "matrix" object as argument. 
## It then determines whether the inverse of the matrix already exists or not.
## If inverse already exists, then it simply gets the cached copy, and returns it.
## If however, the inverse doesn't already exist, the function computes the inverse,
## stores it in cache, and returns it. As a special case, if the matrix is singular, 
## i.e. the determinant of marix is 0, the function geenrates a message saying the 
## matrix is singular, and sets the inverse to NULL.


cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    
    if(!is.null(inv)){
        message("Getting cached Inverse.")
        return(inv)
    }
    
    mat <- x$get()
    
    if(det(mat)==0){
        message("Matrix is singular. Cannot compute inverse.")
        inv <- NULL
    }
    else{
        inv <- solve(mat)
    }
    
    x$setinv(inv)
    inv
}
