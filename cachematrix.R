## The function cache a matrix of concern and the inverse of it 

## makeCacheMatrix is a list of 4 functions
## Takes in a numeric matrix
## Always sets the holding variable to NULL everytime the function is called
## 4 functions serve the following purpose:  
##      setmatrix stores/cache the defined numeric matrix
##      getmatrix prints the cached numeric matrix
##      inv_matrix stores/cache the inverse of the defined numeric matrix
##      getinv_matrix prints the cached inversed numeric matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        
        setmatrix <- function(y) {
                x <<- y
                mat <<- NULL
        }
        
        getmatrix <- function() x
        
        inv_matrix <- function(inv) mat <<- inv
        
        getinv_matrix <- function() mat
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             inv_matrix = inv_matrix,
             getinv_matrix = getinv_matrix)
}




## Creates the inverse of the defined numeric matrix of the makeCacheMatrix function

## cacheSolve takes in a function 
## The function must contain functions as created in makeCacheMatrix 
## cacheSolve attempt to inverse the matrix defined in makeCacheMatrix
## It will first check if a inverse matrix was previously stored/cahced and will return the value if found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat <- x$getinv_matrix()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$getmatrix()
        mat <- solve(data, ...)
        x$inv_matrix(mat)
        mat
}
