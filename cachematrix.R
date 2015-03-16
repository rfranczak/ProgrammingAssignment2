## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
		## This is inverse matrix to x
		inv_x <- NULL
		
		set <- function(y) {
			x <<- y
			inv_x <<- NULL
		}
		
		get <- function() x
		
		setinv <- function(inv) inv_x <<- inv
		
		getinv <- function() inv_x
		
		list(set = set, get = get, 
			 setinv = setinv, getinv = getinv)	
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinv function.
##
## other parameters '...'  are pased to the solve() function. Those parameters
## are not used when value is retrived from cache.
## One can call cacheSolve(m,par1)  which will calculate inv_x <- solve(m,par1)
## when calling again cacheSolve(m,par2) function will return value of solve(m,par1) not solve(m,par2)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x
}

   