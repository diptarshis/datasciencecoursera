##This function here will create the cache every time there is a 
##function call

makeCacheMatrix <- function(x = matrix()) {

  ##This part of the function is important for initialization
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #This part of the function retrieves the matrix
  get <- function() x
  
  #This part stores the cache
  setinv <- function(inv) m <<- inv
  
  #This part retrieves the cache
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function will return the cached output if already stored


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Retrieve the cache
  m <- x$getinv()
  
  #If cache is not empty return the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Get the value of the input matrix
  data <- x$get()
  
  #Solve for the inverse
  m <- solve(data)
  
  #Setting the inverse in memory
  x$setinv(m)
  #Return the inverse
  m
}

#Testing the functions

#Creating the square matrix
a <- matrix(rnorm(9),nrow = 3,ncol = 3)

#Creating the cache
ghy <- makeCacheMatrix(x = a)

#Running the cacheSolve function
cacheSolve(x = ghy)


