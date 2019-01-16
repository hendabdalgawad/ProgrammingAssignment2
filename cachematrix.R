makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  if (!nrow(x)==ncol(x)|det(x) == 0){
    print("This matrix is not invertible")
    x<<-NULL
  } 
  mset<-function(y=matrix()){
    if (!nrow(y)==ncol(y)|det(y) == 0){
      print("This matrix is not invertible")
      x1<<-NULL
    } else {
      x1<<-y
    }
    x<<-x1
  }
  m<-x
  mget<-function()m
  invset<-function(inverse) {inv<<-inverse}
  invget<-function()inv
  list(mset = mset, mget = mget, invset = invset, invget=invget)
}


## cacheSolve is used to cache the inverse matrix given by makeCacheMatrix().

cacheSolve <- function(x, ...) {
  inv<-x$invget()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  ## Return a matrix that is the inverse of 'x'
  inv<-solve(x$mget())
  x$invset(inv)
  inv
}
