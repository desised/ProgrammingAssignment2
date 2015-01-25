## The functions below work together to take any numeric matrix as an input 
## and return its inverse, while making use of the cache to store these
##to test, run these nested, for example: cacheSolve(makeCacheMatrix(matrix(data=c(2,2,3,2), 2, 2)))
##also when using a non-unity matrix, 
##pass the second matrix as follows cacheSolve(makeCacheMatrix(matrix(data=c(2,2,3,2), 2, 2)), matrix(data=c(-1,1,3/2,1), 2, 2))


## Creates a list of functions that store and retrieve data in/from cache

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # a variable called "inv", making sure it's clear
  
  #a function that sends the parent function's values to the cache, names need to differ
  set <- function(y) {
    x <<- y #assigning the function parameter to a var called x in the cache
    inv <<- NULL # initiate a var inv in the cache, making sure it's clear
  }
  get <- function() x #creating a function called "get" that returns the value of x
  setinv <- function(inverse) inv <<- inverse # function sends the inverse matrix to the cache, stored in a variable called "inv"
  getinv <- function() inv #function called getinv retrieves the inverse from the cache
  
  #place functions in a list, return list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Finds and returns the inverse of a matrix, starting the search with the cache, and runs a computation
## only if none are available in the cache. Inherits arguments from solve()

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv() #call getinv() function from list set by makeCacheMatrix function
  #if it's not empty, retrieve the inverse from memory
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if it is emply, go on to calculate the inverse
  data <- x$get() # get the origianl matrix from cache
  inv <- solve(data, ...) #find the inverse matrix of x, store in var called "inv"
  x$setinv(inv) #store the inverse in the cache
  inv # Return the inverse matrix of 'x'
}


