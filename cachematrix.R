## Cahce inverse of a matrix
## 2 functions below make obect which store matrix 
## and calculate its inverse with caching.

## makeCacheMatrix funtction creates "matrix" 
## object that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  inver_m<-NULL
  set<-function(y){
    x<<-y
    inver_m<<-NULL
   }
  get<-function() x
  setInvMat<-function(inverse) inver_m<<- inverse
  getInvMat<-function() inver_m
  list(set=set, get=get,
       setInvMat=setInvMat,
       getInvMat=getInvMat)
}

## Function cahceSolve computes inverse of special "matrix" from 
## makeCacheMatrix function indicated above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver_m<-x$getInvMat()
  if(!is.null(inver_m)){
    message("getting cached data")
    return(inver_m)
   }
  matrix <- x$get()
  inver_m<-solve(matrix, ...)
  x$setInvMat(inver_m)
  inver_m
  
}
