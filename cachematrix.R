## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to calculate a matrix inverse and cache the the results
# for later use
makeCacheMatrix <- function(x = matrix()) {
	# Set null variable to receive the matrix inverse
	m <- NULL
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	# Get the user defined matrix
	get <- function() x
	# Define the function to solve the user matrix inverse
	setmatrix <- function(solve) m <<- solve
	# Get the matrix inverse
	getmatrix <- function() m
	list(set = set, get = get, setmatrix = setmatrix, 
		getmatrix = getmatrix)
}


## Write a short comment describing this function

# Function to retrive the calculated matrix inverse. If there is no
# resolution cached it solves the matrix and cached it result using
# arguments from the makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	# Check if there is a cached result
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## If not, invert the matrix and cache the result
	# Set the matrix passed by the makeCacheMatrix() function
	data <- x$get()
	# Invert the matrix
	m <- solve(data)
	# Set the inverted matrix to 'm' object
	x$setmatrix(m)
	# Return 'm' object
	m
}
