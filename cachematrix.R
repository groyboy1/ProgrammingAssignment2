## These 2 functions are aimed to calculate the matrix inverse and cache the result. 
## If the matrix is the same the future calls to get the value of the matix inverse 
## is returned from the cache instead of calculating it again.
## This is because for a big sized matrix it can be a costly operation.
## Using the <<- operator the values are saved outside the current environment.
## To set a new matrix you use makeCacheMatrix$set. This nullifies(sets to NULL) the cached matrix inverse.
## The inverse is then recalculated in the next call of cacheSolve and cached again for future.  

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions:
## to set and get the values of the matix and it's inverse 
makeCacheMatrix <- function(x = matrix()) {
    matrixInverseCached <- NULL
    
    ## Cache the matrix passed as parameter y
    ## NUllify the cached matrix inverse
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
    
    ##  return a list of the above functions
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
    
    ## if cached inverse exists return inverse
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    
    ## get matrix
    matrix <- x$get()
    
    ## compute inverse
    matrixInverse <- solve(matrix)
    
    ## save inverse for future
    x$setMatrixInverse(matrixInverse)
    
    ## return inverse
    matrixInverse
}