## I have set x as the matrix and z as the null. 
##Then I get the respective matrix and null.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
z<-NULL
set<-function(y){
  x<<-y
  z<<-NULL
}
get<-function()x
setinverse<-function(inverse) z<<-inverse
getinverse<-function()z
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#this function calculates the inverse of the matrix. 
##If it is computed already, the function will get it from the cache. 
##Otherwise, it will compute the inverse and set the cache value equal to it using setinverse.

cacheSolve <- function(x, ...) {
       z<-x$getinverse()
       if(!is.null(z)){
         message("getting cached inverse")
         return(z)
       }
       data<-x$get()
       z<-solve(data,...)
       x$setinverse(z)
       z
}
