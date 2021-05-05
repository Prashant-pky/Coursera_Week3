makeInverse <- function(x = matrix()) {
  inverse <- NULL
  
  ### set function in case we want to redefine new matrix 
  set <- function(y) {
    x <<- y ## <<- operator becasue we are changing variable of parent(same global in this case) enviroment in child enviroment 
    inverse <<- NULL
  }
  
  ###returning value of defined vector or whatever we set in set function (last line expression is return value)
  get <- function() x
  
  #### defining the inverse value if it is available
  setinv <- function(z) inverse <<- z
  ### returning back the inverse value...
  getinv <- function() inverse
  ##creating list of function....can be called with $ sign
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## Cachemean two step inside...one for checing cache and returning inverse if availbale ....second to evalute
## Cacheman need makeInverse type object as input otherwise x$get or x$getinv  would not work
cacheinverse <- function(x, ...) {
  inverse <- x$getinv()      ### due to lexial scoping it will go to enviroment where it is defined and return stoared m value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)  ### if data available function ends here only ....no further executing
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)   ## set new inverse ...can be use for next time use
  inverse  ## return value
}


