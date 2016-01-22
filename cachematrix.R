## The two functions below enable you to create a special form of a matrix
## that allows you to solve and cache the inverse of that matrix.
## Once the inverse has been cached, the solution can be found by retrieving the cache
## rather than solving the inverse again.

## This function creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function () i
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)

}


## This function returns the inverse of matrix 'x'
## It first checks to see if the inverse has already been solved and stored in the cache.
## If so, it returns the cached solution.
## If not, it computes the inverse, stores it in the cache, and returns the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
        	message("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
