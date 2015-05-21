## These two functions in combination create a special vector
## imitating a matrix that can return and cache its inverse
## Example use:
    ## mat <- makeCacheMatrix()
    ## mat$set(matrix(rnorm(25), 5, 5))
    ## inverseMatrix <- cacheSolve(mat)


## Creates the special vector that allows
## its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
    #vector that will contain cached inverse
    inv <- NULL
    
    #set and get the original matrix (x)
    get <- function() x
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    #set and get the inverse cache
    setInvMatrix <- function(invMatrix) inv <<- invMatrix
    getInvMatrix <- function() inv
    
    #return list with the above functions
    list(set = set,
          get = get,
          setInvMatrix = setInvMatrix,
          getInvMatrix = getInvMatrix)
}


## Calculates and returns matrix's inverse, or 
## returns its previously calclated (and cached) inverse if
## previously calculated

cacheSolve <- function(x, ...) {
    inv <- x$getInvMatrix()
    
    #if the inverse has already been calculated, 
    #return the cached inverse matrix
    if(!is.null(inv)) {
        print("fetching cached data")
        return(inv)
    }
    
    #else, it has not been calculated yet
    #begin calculating inverse
    print("calculating inverse")
    
    mat <- x$get()
    invMat <- solve(mat)
    x$setInvMatrix(invMat)
    invMat
}
