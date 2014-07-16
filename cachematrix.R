## Caching the Inverse of a Matrix
## The code below computed the inverse of matrix
## The computed(or inversed) matrix is then cached
## so that if someone needs the inverse of matrix
## again then it will return the cached inverse matrix
## rather than recomputing the inverse.
##
## How to test this function on R Console:
## > source("cachematrix.R")
## > m_data <- matrix(c(1:4), nrow=2, ncol=2)
## > m_cached <- makeCacheMatrix(m_data)
## > m_inverse <- cacheSolve(m_cached)
## > m_inverse <- cacheSolve(m_cached)
##   (When you call cacheSolve again on same matrix it returns 
##   cached data and display followint message)
## getting cached data  
## > m_inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## To test that we computed correct inverse value of matrix
## try this below in R Console and it will return an identity matrix.
## > m_data %*% m_inverse
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## References
## - solve(a, b, ...) function from R Help manuals
## - Google and Stackoverflow to search function that compute inverse of matrix
## - Forked baseline code from
##   https://github.com/rdpeng/ProgrammingAssignment2
## - R Programming Coursera Course by  Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD

## The function makeCacheMatrix creates a special "matrix" 
## , which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Create a variable 'inverse' and initialize this will null
  ## 'inverse' will store the cached value containing inverse of matrix
  inverse <- NULL
  
  ## set function to set the value of the matrix
  ## when you set an new matrix then set the inverse to null 
  ## so that we recompute the matrix when matrix data changes.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get function to get the value of the matrix
  get <- function() x
  ## setinverse function sets the value of the inverse of matrix
  setinverse <- function(inverse) inverse <<- inverse
  ## 4. get the value of the inverse of matrix
  getinverse <- function() inverse

  ## Create a list of above functions and return this list
  ## This list is basically a special cached matrix that is
  ## created by the function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  The function cacheSolve calculates the inverse of the special "matrix" 
##  created with the above function. However, it first checks to see 
##  if the inverse has already been calculated. If so, it gets the inverse 
##  from the cache and skips the computation. Otherwise, it calculates 
##  the inverse of the matrix and sets the value of the inverse in the cache 
##  via the setinverse function.
##  This function makes use of solve(a, b, ...)
##  The R Help document mentions that if argument 'b' is missing
##  then 'b' is taken to be an identity matrix and solve will return the
##  inverse of argument 'a'.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First get the inverse data stored in the special matrix
  inverse <- x$getinverse()
  
  ## Check if the value of inverse is not null using the is.null function
  ## If inverse is not null then this the cached data and we simply return
  ## the cached data and the function call ends here
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## As the data value of inverse is null, we will compute the inverse
  ## using solve(a, b, ...) function.
  ## we will cache the value of inverse into special matrix x
  ## by calling the x#setinverse(inverse) function
  ## This function will then return the computed inverse of matrix.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
