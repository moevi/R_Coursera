##This function,makeCacheMatrix,create a special "matrix" object and compute its inverse




makeCacheMatrix <- function(x = matrix()) {

inver<-NULL
  set <-function(inverse) {
    
    inverse <<- inver
    inver<<-NULL
    
  }   
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set =set,get=get,setinverse=setinverse,getinverse=getinverse)
  
  

}


## This function get the inverse if it was already and the result was cached calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inver <- x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  ##otherwise compute the inverse of the matrix
  Matrix<- x$get()
  inver <- solve(Matrix, ...)
  ##store the value into the cache
  x$setinverse(inver)
  return(inver) 
}
