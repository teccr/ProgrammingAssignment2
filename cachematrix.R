## Put comments here that give an overall description of what your
## functions do

## This function will create an object to store a matrix and 
## the inverse version of the same matrix.
## Arguments:
## x:	A valid matrix for the inverse operation. 
## return:	The function will return an R object with the following nested functions:
##		set: Set a value for the matrix 
##		get: Get the value of the matrix
##		setsolve: Set the value of the inverse of the matrix
##		getsolve: Get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	## Setting private variable to store the cached inverse of the matrix
	s <- NULL
	## Accesor to set the value of the cached matrix
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	## Accesor to get the value of the cached matrix
	get <- function() x
	## Accesor to set the value of the cache inverse of Matrix
	setsolve <- function(solved) s <<- solved
	## Accesor to get the cache of the inverse of Matrix
	getsolve <- function () s
	## Creating a list (R object) containing all the accessors and setting the names.
	list(set = set, get = get, 
	setsolve = setsolve, getsolve = getsolve)
}

## This function will return the inverse of a Matrix using a cache 
## result.
## Remark:	The Function is using the solve() R function to calculate the inverse.
## 		The First time the function is executed it will calculate the inverse,
##		store the result and return it. Next time it will return the stored value.
## Arguments: 
## x: 	A valid matrix for the inverse operation.
## '...': 	Optional arguments for the solve() function. Check the 
## 		R reference (?solve).
## return:	A Matrix with the inverse of argument x.
cacheSolve <- function(x, ...) {
      ## Get the cached Inverse of the Matrix.
	sTmp <- x$getsolve()
	## If the cache object is different from NA (It was set), 
	## then it will return the value and exit the function.
	if(anyNA(sTmp)) {
		message("getting cached data")
		return(sTmp)
	}
	## If the inverse of the matrix is not in the cache, it will get the
	## matrix data and calculate the inverse of the Matrix using the solve function and
	## any parameters introduced by the programmer (type ?solve in the R console to get help).
	data <- x$get()
	sTmp <- solve(data, ...)
	## Finally, store the calculated Inverse of the Matrix and return the data.
	x$setsolve(sTmp)
	sTmp
}
