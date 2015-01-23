## These 2 functions are aimed to calculate the matrix inverse and cache the result. 
## If the matrix remains unchanged, the future calls to get the value of the matix inverse 
## is returned from the cache instead of calculating it again.
## This is because for a big sized matrix it can be a costly operation.
## To set a new matrix you use the set method in the special "matrix" object returned by makeCacheMatrix. 
## This nullifies(sets to NULL) the cached matrix inverse.
## The inverse is then recalculated in the next call of cacheSolve and the new inverse cached for future.  

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions -
## to set and get the matix and to set and get the inverse 
makeCacheMatrix <- function(x = matrix()) {
    matrixInverseCached <- NULL
    
    ## Cache the matrix passed as parameter y
    ## Nullify the cached matrix inverse
    set <- function(y) {
        x <<- y
        matrixInverseCached <<- NULL
    }
    
    ## Return the cached matrix
    get <- function() x
    
    ## Cache the matrix inverse passed as parameter matrixInverse
    setMatrixInverse <- function(matrixInverse) matrixInverseCached <<- matrixInverse
    
    ## Return the cached matrix inverse
    getMatrixInverse <- function() matrixInverseCached
    
    ## Return a list of the above functions
    list(set = set, 
         get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}


## This function takes the special "matrix" object returned by makeCacheMatrix and computes and returns its inverse. 
## It reads the inverse from the special "matrix" object if its there in it's cache. 
## If not, it computes the inverse and stores it into the special "matrix" object's cache. 
cacheSolve <- function(x, ...) {        
    
    ## Read inverse from cache
    matrixInverse <- x$getMatrixInverse()
    
    ## If cached inverse exists, return inverse
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## Get matrix
    matrix <- x$get()
    
    ## Compute inverse
    matrixInverse <- solve(matrix, ...)
    
    ## Save inverse for future
    x$setMatrixInverse(matrixInverse)
    
    ## Return inverse
    matrixInverse
}