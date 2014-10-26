## Put comments here that give an overall description of what your
## functions do

## create a matrix object for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
	## initialize an attribute to store the inverse of a matrix
	i <- NULL
	## set the attributes of the matrix
	set <- function(matrix){
		m <<- matrix
		i <<- NULL
	}
	
	## get the matrix
	get <- function(){
		m
	}
	
	## set the inverse of the matrix
	setInverse <- function(inverse){
		i <<- inverse
	}
	
	## get hte inverse of the matrix
	getInverse <- function(){
		i
	}
	
	## return the list of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Compute the inverse of a matrix returned by "makeCacheMatrix". If the inverse has been calculated, then cacheSolve should just retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		
		## Return the matrix if it exists
		if(!is.null(i)){
			message("getting cached inverse matrix")
			return(i)
		}
		
		## get the original matrix
		matrix <- x$get()
		
		## calculate the inverse matrix using matrix multiplication
		i <- solve(matrix, ...)
		
		## set the inverse of the matrix to be m
		x$setInverse(i)
		
		# return the inverse
		i
}
