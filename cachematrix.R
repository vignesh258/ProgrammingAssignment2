## This function defines four fucntions to get and set the matrix and its inverse
## This takes in the Matrix to be inverted as the input

makeCacheMatrix <- function(x = matrix()) ## X is the input matrix
{
     ## Mt_Inv is the variable to hold output
     ## Set to Null Initially
     Mt_Inv = NULL                        
     set = function(Mt_Set) #Function to reset the value of x - the input matrix
     {
          x <<- Mt_Set
          Mt_Inv <<- NULL
     } 
     get = function() x     #Function to get the value of x
     #Function to set the inverse value so its value can fetched without recalculating
     setInverse = function(solve) Mt_Inv <<- solve 
     #Function to get inverse value if it is already set
     getInverse = function() Mt_Inv
     #Return a list of all above functions
     list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}



##Function to calculate inverse of the matrix inputted in the above function
cacheSolve <- function(x, ...) 
{
     ## Get the inverse matrix if it is already set
     Mt_Inv = x$getInverse()
     if (!is.null(Mt_Inv)) 
     {
          message("Cached Data")
          return (Mt_Inv)
     }
     ## Caculate the inverse Matrix
     Mt_Inv = solve(x$get()) 
     ## Set the inverse so that it can accessed without calculating the next time
     x$setInverse(Mt_Inv)
     return (Mt_Inv)
}
