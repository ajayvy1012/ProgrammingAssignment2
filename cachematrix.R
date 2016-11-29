## The two functions are created for catching the inverse of a matrix

## A matrix is a collection of data elements in a 2-Dimensional rectangular layout
## For a square matrix, the inverse is written as A^-1. When a matrix A is multiplied by A^-1


makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. 
## We can get an inverse of a square matrix by swapping the coordinates of a and d and by putting
## the negative symbols in front of c and d, then multiplying each coordinate by the determinant
## which can be calculated by 1/(ad - bc), the resultant matrix is the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
