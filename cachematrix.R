## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Initializing Inverse of matrix to 
  InverseM <- NULL
  ##Create another function to check first if the inverse has been calculated/created or to see if the 
  ##means needs to be recalculated based on if the matrix has been changed
  set <- function(y) 
    {
    x <<- y
    InverseM <<- NULL
  }
  
      ##If not creates function to calculate inverse of matrix by the SOLVE function and set the inverseof matrix 
      ##gets inverse value
  get <- function() x
      ##Calculates inverse of mean by the solve function and sets the setInverseM equal to output
  setinverse <- function(solve) InverseM <<- solve
      ##gets the previously calculated value
  getinverse <- function() InverseM
      ##passes the value of the whole function 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




##Create Example Square Matrix

ExampleMat <- matrix(c(sample(1:100, 9)),nrow=3,ncol=3)
##Set an object ("a") equal to output of function  above
a<-makeCacheMatrix(ExampleMat)
## Write a short comment describing this function







cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Check to see if the inverse has already been calculated and return value if so
  InverseM <- x$getinverse()
  if(!is.null(InverseM)) {
    message("getting cached data")
    return(InverseM)
  }
  ##If the inverse of matrix  DNE, calculate and retrieve
  data <- x$get()
  InverseM <- solve(data, ...)
  x$setinverse(InverseM)
  InverseM
}

##Test code 

solve(ExampleMat)
cacheSolve(a)
class(a)
class(cacheSolve)
class(makeCacheMatrix)
