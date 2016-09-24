## 
## file: cachematrix.R
##
## Two R functions in this file:
##
## (1) makeCacheMatrix() creates environment that stores matrix and can store its inverse, and
##     returns list of functions used to set and access those matrices
## (2) cacheSolve() computes or retrieves inverse of matrix stored in environment created by 
##     makeCacheMatrix(), and stores new inverse in that environment

## makeCacheMatrix() creates environment that stores matrix X and can store its inverse I, and 
## returns list of functions Z[1:4] used to...
##
##      [1] (Re)set the stored matrix           Z$set() 
##      [2] Retrieve the matrix                 Z$get()
##      [3] Set inverse of the matrix           Z$setinv()
##      [4] Retrieve inverse of the matrix      Z$getinv()

makeCacheMatrix <- function(X = matrix()) 
{
        ## Creates environment variable I to cache inverse of X
        ## Value NULL indicates to cacheSolve() that inverse not yet computed
        I <- NULL
        
        ## Creates function that can be used to reset value of matrix cached in environment
        ## variable X. Sets value of inverse matrix I to NULL, indicating to cacheSolve() that
        ## inverse has not yet been computed.
        set <- function(Y)
        {
                X <<- Y
                I <<- NULL
        }
        
        ## Creates function to retrieve matrix cached in environment variable X
        get <- function() X
        
        ## Creates function to cache inverse of matrix X in environment variable I
        setinv <- function(INV) I <<- INV
        
        ## Creates function to retrieve inverse matrix I from the environment
        getinv <- function()  I
        
        ## Returns list of functions created above
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() retrieves and returns inverse I from environment associated with list Z.
## Computes and caches inverse if it does not already exist.

cacheSolve <- function(Z) 
{
        ## RETRIEVER code: If inverse matrix associated with list Z is already cached,
        ## prints a message to the conosole, returns the cached inverse, and halts execution.
        ## If inverse associated with list Z is not in cache, execution passes to solver code.
        I <- Z$getinv()
        if(!is.null(I))
        {
                message("getting cached data")
                return(I)
        }
        
        ## SOLVER code: Retrieves matrix X from cache associated with Z, solves for inverse I,
        ## stores I in cache, and returns I
        X <- Z$get()
        I <- solve(X)
        Z$setinv(I)
        I
}
