#  The two functions below will help to more efficiently
#  calculate the inverse of a matrix since it will cache
#  the value of the inverse when it is calculated instead
#  of having to re-calculate the value each time the function
#  is called.


#  makeCacheMatrix defines several functions and returns them in a list:
#  - set: sets x to a new matrix and m to null
#  - get: returns the current value of x
#  - setInverse: sets m to the value passed in to this function
#  - getInverse: returns the current value of m
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) m <<- inverse
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


#  cachSolve gets the value of m, which should be
#            the inverse of the matrix.  If the value
#            is null, then it calculates the inverse
#            of the matrix, sets the value of m to the
#            value that it just calculated and returns m
#            (the inverse of the matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setInverse(m)
     m
}
