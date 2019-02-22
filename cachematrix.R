#This set of functions inverts a matrix and stores the solution in memory for later recall.
#Input: a square, invertable matrix
#Output: inverse of input matrix located in memory cache
#Author: Neil Schaner, 2/22/19

#Create a list of function for setting and getting a matrix and its inverse
makeCacheMatrix <- function(m = matrix()) { 	#input matrix = m
	inv <- NULL 						#create variable for inverse of m
	set <- function(y) { 				#function to assign matrix and inverse to higher environment
		m <<- y
		inv <<- NULL
	}
	get <- function() m 							#function to recall input m
	setinv <- function(invmat) inv <<- invmat 			#function to assign inverse of m
	getinv <- function() inv 						#function to recall inverse of m
	list(set = set, get = get, setinv = setinv, getinv = getinv) 	#return list of functions
}

#Solve for the inverse of a matrix and cache in memory. If solution is cached, retrieve from memory
cacheSolve <- function(mat, ...) { 	#input matrix = mat
	matinv <- mat$getinv() 			#recall inverse of mat from makeCacheMatrix
	if(!is.null(matinv)) { 			#if inverse of mat is in cache, recall and return
		message("getting cached data")
		return(matinv)
	}
	data <- mat$get() 			#if inverse of mat is not in cache, get the matrix
	matinv <- solve(data, ...) 	#calculate inverse of matrix
	mat$setinv(matinv) 			#set the inverse, put in cache
	matinv 					#return the inverse
}