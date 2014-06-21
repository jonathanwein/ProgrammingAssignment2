## makeCachMatrix creates and caches a matrix and whether its inverse is 
## cached or not. 

makeCacheMatrix <- function(x = numeric()) {
    m <<- NULL
    set <- function(y){
        x <<- y
        m <- NULL
    } 
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve recieves an invertible matrix, checks whether it has an 
## inverse cached already: if there is it returns it. if there's non it
## calculates the inverse and returns it.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("using the cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    return(m)
}
