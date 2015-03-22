## The below functions can be used to create matrix object
## and cache the inverse of matrix leveraging the solve()
## It is assumed that the matrix is a sqaure matrix and is invertible
## The function does not handle errors thrown by solve()
## if the matrix is not invertible or not a square matirx 


## The makeCaheMatrix functions takes matrix as argument
## and creates and object with the below functions
## set() to set the value of matrix
## get() which returns the stored matrix from the object inctance
## setInverse() to calculate and cache the inverse of the matrix
## getInverse() to retrieve the inverse matrix
## invMatrix is the variable which caches the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	
	## Set InvMatrix to NULL indicating that setInverse()
	## needs to be called before getInverse() 
	
	invMatrix <- NULL
        
        ## set() to set the value of matrix
        set <- function(y) {
                x <<- y
                
                ## Reset InvMatrix to NULL for any change in matrix
                invMatrix <<- NULL
        }
        
        ##Return stored matrix value
        get <- function() x
        
        ## set the inverse for the matrix
        setInverse <- function(inverse) invMatrix <<- inverse
        
        ## Retunr the stored inverse matric
        getInverse <- function() invMatrix
        
         list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve function takes makeCacheMatrix as argument
## check if the inverse of the matrix is strore in the cache
## is it is in the cache then return the cached inverse matrix
## if not then calculate the inverse, set the cache and return
## the inverse

cacheSolve <- function(x, ...) {
        
         ## Get the cached value for inverse from the object "x"
         ## passed in the argument
         
         invMat <- x$getInverse()
	 
	 	## if the object returns a value for inverse
	 	## it would imply that the precalcculated value 
	 	## for inverse is avaliable
	 	
	 	if(!is.null(invMat)) {
	                message("getting the precached inverse from cache")
	                return(invMat)
	        }
	        
	        ## if the cache is empty then get the matrix from the object
	        ## "x" and calculate the inverse. Set the cache with the calculated
	        ## inverse and return the inverse.
	        
	        tempMatrix <- x$get()
	        invMat <- solve(tempMatrix, ...)
	        x$setInverse(invMat)
        	invMat
                
}
