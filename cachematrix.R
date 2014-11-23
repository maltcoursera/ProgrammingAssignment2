## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     ## sets the inverse to NULL
     inverse <- NULL
	##allows to reset the matrix of a makeCacheMatrix object
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	##return the matrix
	get <- function() x
	## sets the inverse variable to the the inverse matrix of x
	setinverse <- function(solve) inverse <<- solve
	## allows for access to inverse
	getinverse <- function() inverse
	list(set=set, get = get, setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get the inverse variable from a makeCacheMatrix
        inverse <-x$getinverse()
        ## check whether inverse is already calculated, if yes, then return chached inverse with a message
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        ## get the matrix with which makeCacheMatrix was called
        data <- x$get()
        ## calculate the inverse
        inverse <- solve(data,...)
        ##set the inverse inthe makeCacheMatrix
        x$setinverse(inverse)
        ##return the calculated inverse
        inverse
}
