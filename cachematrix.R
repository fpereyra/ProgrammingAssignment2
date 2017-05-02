## The following function achieves the following:
#### set the square matrix
#### get the square matrix
#### set the inverted square matrix
#### get the inverted square matrix

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    get <- function() x
    set_matrix_inverse <- function(inverse) x_inverse <<- inverse
    get_matrix_inverse <- function() x_inverse
    
    list(set = set, get = get, set_matrix_inverse = set_matrix_inverse
         , get_matrix_inverse = get_matrix_inverse)
    
}


## The following function inverts the supplied square matrix, but checks at the outset
## whether it has already been computed and retrieves the cached data where possible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    x_inverse <- x$get_matrix_inverse()
    
    if(!is.null(x_inverse) == TRUE) {
        message("Retrieving cached data...")
        return(x_inverse)
    }
    
    data <- x$get()
    
    x_inverse <- solve(data)
    
    x$set_matrix_inverse(x_inverse)
    
    x_inverse
}
