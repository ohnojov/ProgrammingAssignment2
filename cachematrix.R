## a pair of functions that cache the inverse of a matrix

## The first function, makeMatrix creates a special list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setInverse <- function(solve)  i
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## The following function checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}       

