# This function gets called first and generates a list with 3 functions, getting i , setting and getting inverse
makeCacheMatrix <- function(x = matrix()) {
	i = NULL
        get = function() x
        setinv = function(inverse) i <<- inverse
        getinv = function() i
        list(get=get, setinv=setinv, getinv=getinv)
}

# This function actually calculates the inverse , but first checks if getinv object is not NULL. If not NULL , it calls setinv first before returing.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i = x$getinv()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }

        m = x$get()
        i = solve(m, ...)
        x$setinv(i)
        i
}
