#To cache the inverse of a matrix

#this function helps to cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse<-matrix()
  set<-function(y){
    x<<-y
    inverse<<-matrix()
  }
  get<-function(){
    x
  }
  set_inverse<-function(inverse2){
    inverse<<-inverse2
  }
  get_inverse<-function(){
    inverse
  }
  list<-list(get=get,set=set,set_inverse=set_inverse,get_inverse=get_inverse)
}

#To compute the inverse

cacheSolve <- function(x, ...) {
  inverse<-x$get_inverse()
  if(!is.null(inverse))
  {
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data,...)
  x$set_inverse(inverse)
  inverse
}
