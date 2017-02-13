
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL  ##sets the inv to null, acts as a placeholder for a future value
  set<-function(y){ 
    ##use <<- to assign value to an object in an environment 
    ##different from the current environment
    x<<-y 
    inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function()inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv=x$getinv()
  ##if the inverse has already been calculated
  ##it gets it from the cached data and skips calculations
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data=x$get()
  inv=solve(mat.data,...)
  ##sets the value of the inverse in the cache
  x$setinv(inv)
  return(inv)
}
