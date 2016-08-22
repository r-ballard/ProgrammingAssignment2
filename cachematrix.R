## Put comments here that give an overall description of what your
## functions do
## Matrix calculations can be costly in terms of computational power
## This R script is an attempt to allow the user to compute matrix inverses
## More efficiently by creating a function to cache a matrix which will
## have it's inverse often used, thus bypassing the need to have the inverse calculated
## repeatedly.


## Write a short comment describing this function

## The makeCacheMatrix function contains the set, get, setMatrixInv, and getMatrixInv functions.
## The set function caches the matrix the user will be working with
## The get get function returns the matrix the user will be working with
## The setMatrixInv sets the cached matrix inverse
## The getMatrixInv returns the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
  
  cacheInv<-NULL #initiates the cached matrix inverse as a NULL object
  
  set <- function(y) #function allowing the user to set the matrix which will be solved and have it's solution cached
  {
    x<<-y
    cacheInv<-NULL #resets the cached inverse to a NULL object (as the matrix has changed)
    
  }
  
  get<- function() x #returns the matrix
  setMatrixInv <- function(MatrixInv) cacheInv<<-MatrixInv #sets the matrix inverse, also caches the value input
  getMatrixInv <- function() cacheInv #returns the cached matrix inverse
  list(set = set,get = get, setMatrixInv = setMatrixInv, getMatrixInv = getMatrixInv)
  
}


## Write a short comment describing this function
## The cacheSolve function has a matrix as an argument.
## If there is already a cached matrix inverse then this matrix will be returned.
## If there is not already a cached matrix inverse a matrix inverse is calculated and then cached.

cacheSolve <- function(x, ...) 
{
  cacheInv <- x$getMatrixInv() #assigns cached inverse as the value provided by the matrix inverse function for x
  
  if(!is.null(cacheInv))
  {
    message("getting cached matrix inverse")
    return(cacheInv)
    
    ## Returns a matrix that is the inverse of 'x'
  }
  matrixToInv <- x$get() #assigns the matrix to be inverted as 'x'
  cacheInv <- solve(matrixToInv, ...) #assigns the cached inverse as x's inverse
  x$setMatrixInv(cacheInv) #sets the cached matrix inverse for x as it's inverse
  cacheInv #returns the newly cached inverse
  
  #at this point the cached value can be assigned to a variable and used globally
  
}