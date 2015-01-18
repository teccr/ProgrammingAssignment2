## This function will run a series of tests over the 
## cachematrix.R file. It will print the inverse
## operation of a matrix several times with some test data (R code created for this project).
cachematrix.tests.Run <- function() {
	source("cachematrix.R")
	cachematrix.tests.assert(matrix(c(4,3,3,2), nrow=2, ncol=2), matrix(c(-2,3,3,-4), nrow=2, ncol=2))
	cachematrix.tests.assert(matrix(c(5,-7,2,-3), nrow=2, ncol=2), matrix(c(3,-7, 2,-5), nrow=2, ncol=2))
	cachematrix.tests.assert(matrix(c(-3,5,1,-2), nrow=2, ncol=2), matrix(c(-2,-5,-1,-3), nrow=2, ncol=2))
}

## This function will execute the inverse of a matrix and print both matrices in the screen.
## Arguments:
## 	originalMatrix: The matrix that will be executed in the inverse operation.
##	The matrix has to be a valid matrix for the inverse operation.
##	inverseMatrix: The reference inverse of the Matrix we want to test.
cachematrix.tests.assert <- function(originalMatrix, inverseMatrix) {
	print("------------------------------------------")
	mTmp <- makeCacheMatrix(originalMatrix)
	invTmp <- cacheSolve(mTmp)
	print("Calculated Inversed of Matrix:")
	print(invTmp)

	if(anyNA(invTmp)) {
		# not calculated, raise error
		print("Error: Inverse of the Matrix was not calculated!")
		print("------------------------------------------")
		return
	}
	
	print("Reference Inverse of the Matrix")
	print(inverseMatrix)

	print("------------------------------------------")
}