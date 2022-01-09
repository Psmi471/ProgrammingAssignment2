## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The purpose create a function containing the original matrix and 
## a space for its inverse to be cached
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<-function() x
  setInv<-function(pass) {m<<-pass}
  getInv<- function() {m}
  list(set = set, get = get, setInv = setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getInv()
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
  
  
}
