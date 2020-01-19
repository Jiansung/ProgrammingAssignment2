## Creating a matrix with makeCacheMatrix then solving it with cache solve 
## automatically finds the inverse information before calculating it again.


makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x) {
  iv <- x$getinverse()
  if (!is.null(iv)){
    message ("getting cached data")
    return (iv)
  }
  data <- x$get()
  iv <- solve(data)
  x$setinverse(iv)
  iv
}


## so, for example, 
## s <- matrix(1:4,2,2)
## t <- matrix(c(2,4,5,6),2,2)

## s1 <- makeCacheMatrix(s)
## t1 <- makeCacheMatrix(t)

## cacheSolve(s1)
## cacheSolve(t1)
## cacheSolve(s1)

## returns

# > cacheSolve(s1)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(t1)
# [,1]   [,2]
# [1,] -0.75  0.625
# [2,]  0.50 -0.250
# > cacheSolve(s1)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5