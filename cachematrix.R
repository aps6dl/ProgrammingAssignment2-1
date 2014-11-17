## makecachematrix: function creates a special 'matrix' object that can cache its inverse
## using lexical scoping
## cachesolve: function computes the inverse of the special 'matrix' returned by makecachematrix
## above. If the inverse has already been calculated and the matrix has not changed then 
## cachesolve retrieves inverse from cache.

## makecachematrix is a function that makes a 'special' list
## containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  my_matrixinverse <- NULL          ## set the value of the matrix inverse
  set <- function(my_matrix) {
          x<<- my_matrix            ## get value of matrix allowing for lexical scoping
          my_matrixinverse<<-NULL       ## set value of inverse allowing for lexical scoping
  }
  get<- function() x                ## gets the matrix 
  
  setinverse<- function(my_matrixinverse) my_matrixinverse<<- solve(x) 
  ## line 20 (above): my_matrixinverse set through lexical scoping
  
  getinverse<- function() my_matrixinverse   ## print my_matrixinverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  ## line 25(above) function value return
}


## the following function calculates inverse of special 'matrix'.
## it first checks whether the inverse has already been calculted.
## if so it gets the inverse from the cache and skips the computation.
## otherwise it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  my_matrixinverse<- x$getinverse()
        ## Return a matrix that is the inverse of 'x'
  
  if (!is.null(my_matrixinverse)) { ##line 43 (here) if non empty my_matrixinverse then          
    message ("getting cached data")
  
    return(my_matrixinverse)        ##end call, my_matrixinverse goes into local environmnent      
  }
  
  data<-x$get()
  my_matrixinverse<-solve(data, ...)
  
  x$setinverse(my_matrixinverse)
  my_matrixinverse
}
