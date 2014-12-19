## Returns a list containing methods to get and set a (matrix) value
## and to get and set a cached operation on that value.
## Setting a new value will clear the cache.
##
## Args:
##   x: A matrix that initialises the returned object with a value
## Returns:
##   A list with methods set, get, setcache and getcache

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setcache <- function(i) cache <<- i
    getcache <- function() cache
    list(set=set, get=get,
         setcache=setcache,
         getcache=getcache)
}


## Returns the inverse of given matrix. Subsequent calls will return the
## cached result. Assumes given matrix is solvable.
##
## Args:
##   x: A matrix, represented by makeCacheMatrix list object
## Returns:
##   The inverse of x

cacheSolve <- function(x) {
    i <- x$getcache()
    if (is.null(i)) {
        # cache is empty
        i <- solve(x$get())
        x$setcache(i)
    }
    i
}
