## Put comments here that give an overall description of what your
## functions do

# The two functions takes a matrix and creates the inverse of that matrix

## Write a short comment describing this function

# This function takes a matrix as input and creates a temporary
# storage for the inverse of that matrix.
# It also creates 4 get/set functions to be accessed by the
# cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  # initialise cached inverted matrix var to NULL
  invCache <- NULL
  
  # old matrix replaced by new matrix
  set <- function(old2new = matrix())  {
    x <<- old2new
    invCache <<- NULL
  }
  
  # returns new matrix, no biggie
  get <- function() x
  
  # inverted matrix => cache
  setInv <- function(invSolve = matrix()) {
    invCache <<- invSolve
  } 
  
  # returns cachedInv, no biggie
  getInv <- function() invCache
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

# This function either retrieves the cached inverted matrix from the
# makeCacheMatrix function using the getInv function, or calculates the
# inverse matrix by use of the solve() and get() functions.
# The inverted matrix is then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverted <- x$getInv()
  
  # is the inverted matrix NULL?
  # if not, yay, we return it
  if (!is.null(inverted))  {
    message("getting cached matrix")
    return(inverted)
  } # "else"
    # yikes, it's NULL!
    # we need to work on this...
    message("forgot to cache, brb")
  
    # getting the matrix for calculation
    data <- x$get()
  
    # calculate inverted
    inverted <- solve(data, ...)
    
    # caching the inverted
    x$setInv(inverted)
    
    # here's the answer
    inverted
 # }
}

## Testing of functions
## > mat <- matrix(c(1,2,4,5,6,7,8,9,10),3,3)
## > mat
## [,1] [,2] [,3]
## [1,]    1    5    8
## [2,]    2    6    9
## [3,]    4    7   10
## > x1 <- makeCacheMatrix(mat)
## > cacheSolve(x1)
##           [,1]      [,2]      [,3]
## [1,]  1.000000 -2.000000  1.000000
## [2,] -5.333333  7.333333 -2.333333
## [3,]  3.333333 -4.333333  1.333333