## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	ivm <- NULL
	set <- function(y) {
		x <<- y
		ivm <<- NULL
	}
	get <- function() x
	setIvm <- function(inverse) ivm <<- inverse
	getIvm <- function() ivm
	list(set = set, get = get,
		 setIvm = setIvm, 
		 getIvm = getIvm)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	ivm <- x$getIvm()
	if(!is.null(ivm)) {
		message("getting cached data")
		return(ivm)
	}
	data <- x$get()
	ivm <- solve(data, ...)
	x$setIvm(ivm)
	ivm
}
