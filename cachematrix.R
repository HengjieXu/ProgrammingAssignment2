## This project is aimed to cache the operation of inversing a matrix with the
## following two functions.

## The first function , makeCacheMatrix creates a list containing the functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL
			
			set <- function(y){
				x <<- y
				inv <<- NULL
			}
			
			get <- function(){
				x
			}
			
			setinverse <- function(inverse){
				inv <<- inverse
			}
			
			getinverse <- function() {
				inv
			}
			
			list(set = set, get = get,
				 setinverse = setinverse,
				 getinverse = getinverse)
			
			
}


## The second function calculates the inverse of the matrix created with the first
## function. It first checks whether the inverse has already existed or not. If it
## has already existed, retrive the inverse. Otherwise, compute the inverse of the
## matrix.

cacheSolve <- function(x, ...) {
       	inv <- x$getinverse()
       	if(!is.null(inv)){
       		message("getting cached data")
       		return(inv)
       	}
       	data <- x$get()
       	inv <- solve(data, ...)
       	x$setinverse(inv)
       	inv
}
