##
## makeCacheMatrix() returns an object that stores a matrix and can store its inverse
##
## cacheSolve() computes the inverse of a matrix stored in an object created by makeCacheMatrix()
##      and stores that inverse in the object

## Given a matrix 'X' to store, makeCacheMatrix(X) creates an object that stores the matrix
## X and can store its inverse I, and a list Z[1:4] that can...
##
##      set the stored matrix                 Z$set() 
##      retrieve the matrix                   Z$get()
##      set the inverse of the matrix         Z$setinv()
##      retrieve the inverse of the matrix    Z$getinv()

makeCacheMatrix <- function(X = matrix()) 
{
        ## Creates function environment variable I to store inverse of X
        ## Value NULL used to indicate inverse has not yet been computed
        I <- NULL
        
        ## Creates function that can be used to set value of stored matrix X
        ## Sets value of inverse matrix I to NULL, used in cacheSolve() to indicate
        ##      inverse has not yet been computed
        set <- function(Y)
        {
                X <<- Y
                I <<- NULL
        }
        
        ## Creates function to retrieve stored matrix X
        get <- function() X
        
        ## Creates function to store inverse I of matrix X
        setinv <- function(INV) I <<- INV
        
        ## Creates function to retrieve inverse matrix I
        getinv <- function()  I
        
        ## Returns list of functions created above
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(Z) 
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
