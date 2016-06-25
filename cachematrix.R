## Please open full screen :)
## Rather than computing the inverse of a given matrix everytime you need
## it, we can cache this inverse matrix using a pair of functions as shown below. 

## The first function creates a special "matrix" that can cache its inverse.
## The second calculates the inverse "matrix" (given by the function one)
## or just retrieves it (from the cache) if it has already been computed.
## NB: we suppose the matrix has not changed and that it's an invertible 
## square matrix.

## the makeCacheMatrix takes a matrix as an argument and returns a "matrix" object
## (which is a list actually) that can cache its inverse. The list containts
## two functions that sets and gets the value of the matrix and two others that sets and gets
## the value of its inverse.
## You can store a matrix (in the list) via makeCacheMatrix()$set(*)
## where * is the matrix you want to store.

makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL
           set <- function(y){
                   x <<- y
                   inv <<- NULL
           }
           get <- function() x
           setinverse <- function(solve) inv <<- solve
           getinverse <- function() inv 
           list(set = set , get = get , setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function returns a matrix that is the inverse of x (ie. the list
## produced by the function makeCacheMatrix. 
##First, it checks if the inverse has been calculated. If yes, it returns the value 
## from the cache. If not, it calculates the inverse of the matrix and stores it
## in the cache (via the setinverse function).
## To run this function, you write (after you stored a matrix!!):
##  cacheSolve(makeCacheMatrix())

cacheSolve <- function(x, ...) {
           inv <- x$getinverse()
           if(!is.null(inv)) {
                   message("getting cached data")
                   return(inv)
           }
           data <- x$get()
           inv <- solve(data, ...)
           x$setinverse(inv)
           inv
}
## Example: 
## store a matrix (square and invertible): start <- makeCacheMatrix() and start$set(matrix(3:6,2,2))
## run cacheSolve(start): gives you the inverse of the matrix
## run again cacheSolve(start): gives you the inverse with the message 
## "getting cache data" (because you stored it in the cache!)
