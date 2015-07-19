## cacheMatrix.R
## ---------------
## makeCacheMatrix(m = matrix())
## cacheSolve
## ---------------
## Jonathan Beamer
## July 19, 2015
## ---------------
## Functions to speed up multiple calls to solve() to invert a matrix using caching
## ---------------
## usage example:
##   setwd("~/rprog/ProgrammingAssignment2/")
##   source("cachematrix.R")
##   m <- matrix(data = c(2,4,6,2, 3,8,9,3, 2,7,8,3, 8,0,3,2), nrow=4, ncol=4)
##   m
##   solve(m)
##   cm <- makeCacheMatrix(m)
##   cacheSolve(cm)
##   cacheSolve(cm)

## makeCacheMatrix
## ---------------
## This function creates a special "matrix" object that can cache its inverse.
## 
makeCacheMatrix <- function(myMatrix = matrix()) {
	myInverse <- NULL
	set <- function(m) {
		myMatrix  <<- m
		myInverse <<- NULL
	}
	get <- function() {
		myMatrix
	}
	setinverse <- function(inverse) {
		myInverse <<- inverse
	}
	getinverse <- function() {
		myInverse
	}
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve
## ---------------
## This function calculates the inverse (using solve()) of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinverse 
## function.
## 
cacheSolve <- function(m, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- m$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	message("calculating inverse and caching it for future calls:")
	inverse <- solve(m$get(), ...)
	m$setinverse(inverse)
	inverse
}
