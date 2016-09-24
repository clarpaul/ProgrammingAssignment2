## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(X = matrix()) 
{
        I <- NULL
        set <- function(Y)
        {
                X <<- Y
                I <<- NULL
        }
        get <- function() X
        setinv <- function(INV) I <<- INV
        getinv <- function()  I
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(Z, ...) 
{
        I <- Z$getinv()
        if(!is.null(I))
        {
                message("getting cached data")
                return(I)
        }
        MAT <- Z$get()
        I <- solve(MAT)
        Z$setinv(I)
        I
        ## Return a matrix that is the inverse of 'Z'
}
