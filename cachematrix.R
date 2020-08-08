## Put comments here that give an overall description of what your
## functions do
## caching the inverse of a matrix rather than compute it repeatedly
## there are two  functions makeCacheMatrix 
## makeCacheMatrix consists of  set, get, setinverse, getinverse


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL                 ## initializing inverse as NULL
  set<-function(y){
      x <<- y
      inv <<- NULL          
  }
  get <- function(){x}                 ## function to get matrix x 
  setinverse <- function(inverse){inv <<- inverse}
  getinverse <- function() {inv}                   ## function to obtain inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function
## this used to get the cache data

cacheSolve <- function(x, ...) {    ## gets cache data
      inv <- x$getinverse()
      if(!is.null(inv)){               ## checking wether inverse is NULL
             message("getting cached data")
             return(inv)                ## returns inverse value
      } 
    
       
      mat <- x$get()
      inv <- solve(mat,...)           ## calculates inverse value
      x$setinverse(inv)
      inv                      ## Return a matrix that is the inverse of 'x'
      
}

