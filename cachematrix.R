## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeated.The following functions are used to cache
## the inverse of a matrix.
## Updated Forked version of cachematrix on 22-MAR-2015



##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##Function Details 
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse of matrix
##4.get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed),then the 
##cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'

    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m

}

## OUTPUT 
## x = rbind(c(1, -1/4), c(-1/4, 1))
## m = makeCacheMatrix(x)
## cacheSolve(m)
##        [,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
## cacheSolve(m)
##getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667