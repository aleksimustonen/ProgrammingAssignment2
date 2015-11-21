## 

# sources:
# http://www.statmethods.net/advstats/matrix.html -- R Matrix Algebra Cheatsheet
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/matrix.html -- matrix() function definition

## makeCacheMatrix Creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- x
    cachedInverseMatrix <- NULL
    
    # Use 'get' to get the matrix value
    get <- function() cachedMatrix
    
    # Use 'getInv' to get the inverse matrix value
    getInv <- function() cachedInverseMatrix
    
    # Use 'setInv' to set a new value for the inverse matrix. Does not touch actual matrix value.
    setInv <- function(newInverseMatrix) {
        cachedInverseMatrix <<- newInverseMatrix
    }
    
    
    list(get=get, getInv=getInv, setInv=setInv)
}


## cacheSolve Computes the inverse of the special "matrix" returned by makeCacheMatrix or returns a cached result

cacheSolve <- function(x, ...) {
    cacheMiss <- is.null(x$getInv())
    
    if(cacheMiss){
        print("        Cache miss. Calculating inverse...")
        inverse = solve(x$get())
        x$setInv(inverse)
    } else {
        print("        Cache hit. Returning precalculated inverse...")
    }
    
    x$getInv()
}

#Usage:
demo <- function() {
    writeLines("\n\n\n\n\n")
    print("Creating a new matrix 'basicMatrix' to be used as input...")
    basicMatrix <- matrix(c(1, 2,  3, 4), nrow=2, ncol=2)
    print(basicMatrix)
    print("Making a cached matrix 'cacheMatrix' from the input...")
    cacheMatrix <- makeCacheMatrix(basicMatrix)
    print("cacheMatrix$get should return the value of basicMatrix.")
    print(cacheMatrix$get())
    print("cacheMatrix$get should not yet contain the inverse of basicMatrix.")
    print(cacheMatrix$getInv())
    print("Using cacheSolve to solve the inverse of basicMatrix.")
    cacheSolve(cacheMatrix)
    print("cacheMatrix should now contain a cached inversse of basicMatrix.")
    print(cacheMatrix$getInv())
    print("Using cacheSolve again returns the cached inverse.")
    cacheSolve(cacheMatrix)
}

demo()


