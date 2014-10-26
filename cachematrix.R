## Put comments here that give an overall description of what your
## functions do

## create a matrix object for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
	## initialize an attribute to store the inverse of a matrix
	inverseMatrix <- NULL
	## set the attributes of the matrix
	set <- function(a){
		x <<- a
		inverseMatrix <<- NULL
	}
	
	## get the matrix
	get <- function(){
		x
	}
	
	## set the inverse of the matrix
	setInverse <- function(inverse){
		inverseMatrix <<- inverse
	}
	
	## get the inverse of the matrix
	getInverse <- function(){
		inverseMatrix
	}
	
	## return the list of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Compute the inverse of a matrix returned by "makeCacheMatrix". If the inverse has been calculated, then cacheSolve should just retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverseMatrix <- x$getInverse()
		
		## Return the inverse matrix if it exists
		if(!is.null(inverseMatrix)){
			message("getting the cached data of inverse matrix")
			return(inverseMatrix)
		}
		
		## if the inverse is not there, then get the original matrix, calculate the inverse, and then return the inverse matrix
		data <- x$get()
		
		## calculate the inverse matrix using matrix multiplication
		inverseMatrix <- solve(data, ...)
		
		## set the inverse of the matrix to be m
		x$setInverse(inverseMatrix)
		
		# return the inverse
		inverseMatrix
}
