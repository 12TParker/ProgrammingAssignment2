## These functions are used to first creat a special object that stores a numeric matrix
## and then to compute, cache and return its inverse.

## This function is list of functions which: sets the value of the matrix, gets the value of the matrix
## sets the inverse of the matrix and then gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                   #initiate the variable m will null value
  set <- function(y) {                            #within makeCacheMatrix fn initiate a sub fn called set
    x <<- y                                         #set the cached value of x to y
    inv <<- NULL                                      #set the cached value of m to null
  }
  get <- function() x                             #within makeCaccheMatrix fn define sub fn called get
  setinverse <- function(solve) inv <<- solve       #within makeCaccheMatrix fn define sub fn called setinverse 
                                                    #and set cached value of inv to the inverse of x
  getinverse <- function() inv                      #within makeCaccheMatrix fn define sub fn called getinverse
  list(set = set, get = get,                        #define the parent fn as a list of all sub functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns a matrix that is the inverse of x. First it checks the cache to see
## if the inverse has already computed. If not, it calculates the inverse and sets the value of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                        #calculate the inverse of x and set local value of inv by calling the fn getinverse
  if(!is.null(inv)) {                          #if inv is not null then the value has already been calculated so,
    message("getting cached data")            #get cached value of inv
    return(inv)
  }                                           #else...
  data <- x$get()                                  #set value of data to the matrix in x
  inv <- solve(data, ...)                          #set value of inv to the inverse of the matrix in data
  x$setinverse(inv)                                #set cached value of inv to the inverse of the matrix
  inv                                              #display the value of inv
}
