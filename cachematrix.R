##takes a square vector and creates room to cache the inverse. then calculates &
##stores the inverse in a cache or pulls the info out of the cache if exists
##already.

##The purpose create a function containing the original matrix and 
## a space for its inverse to be cached. As well as having a set of set and get
##for the original and inverse when fed into other functions.
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


##This pulls in all the variables and internal functions of makeCacheMatrix().
##it combs though to see if there is a cached inverted matrix and gets the
##original. If its cached already it displays the cache, if not it
##calculates the inverse and displays it and saves it to the cache at the
##pointer for the makeCacheMatrix call.
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
