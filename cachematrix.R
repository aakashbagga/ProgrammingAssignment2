makematrixVector <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
   
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


cacheinverse <- function(x) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setinverse(s)
    s
}
cacheinverse(makematrixVector( matrix(c(1,2,12,13), nrow = 2, ncol = 2)))
