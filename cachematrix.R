## This pair of functions will create a matrix object and
## cache the inverse of this matrix 

## This function creates a special "matrix" object that can cache its inverse.
## From now on this object will be referred to as a 'new environment'

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL                ## An empty inverse (for now in Global)
     setmat <- function(y) {
             x <<- y            ## whatever matrix (x) we passed to our
                                ## makeCacheMatrix() is set to the new
                                ## environment
             inv <<- NULL       ## So is the inv object (it is empty until
                                ## setinv() fills it with an inverse)
     }
     getmat <- function() x
     setinv <- function(inverse) inv <<- inverse        ## we can see setinv()
                                                        ## in action in the
                                                        ## cacheSolve() function
                                                        ## caching the inverse
                                                        ## once it has been
                                                        ## calculated for the
                                                        ## first time
     getcache <- function() inv
     list(setmat = setmat,
          getmat = getmat,
          setinv = setinv,
          getcache = getcache)  ## this list creates the functions as objects in
                                ## the new environment
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getcache()
        if(!is.null(inv)) {
                message("Retrieving cached inverse")
                return(inv)
        }
        mat <- x$getmat()
        inv <- solve(mat)
        x$setinv(inv)           ## this caches the inverse in the environment
                                ## from makeCacheMatrix()
        return(inv)
}

## If x$setinv() has already cached an inverse, the if() function in
## cacheSolve() will return TRUE and the cached inverse will be retrieved




                        ### READ ME ###

## I actually wrote the cacheSolve() function first and therefore knew that I
## needed five objects (x, mat, getmat(), getcache(), setinv()) in the
## makeCacheMatrix() "environment". In that environment, I also need a setmat()
## function which creates the object mat. The first time I evaluate a matrix
## in the new environment, it will be assigned to mat. Hence, I need six objects
## (x, mat, getmat(), getcache(), setinv(), setmat()) in the makeCacheMatrix()
## environment.

## We can test whether these two functions work.
## Let's create a matrix test_matrix
test_matrix <- matrix(c(1, 2, 3, 4), 2, 2)
## Let's introduce it into the makeCacheMatrix() environment
makeCacheMatrixobject <- makeCacheMatrix(test_matrix)
## And run the cacheSolve() function in that environment
cacheSolve(makeCacheMatrixobject)
## To see if this is really the inverse, we can multiply the two matrices
## together to get the matrix:
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
test_matrix %*% cacheSolve(makeCacheMatrixobject)
## The fact that we see "Retrieving cached inverse" means setinv() has worked,
## and that cacheSolve() retrieved the cached inverse and the multiplied it by
## test_matrix.