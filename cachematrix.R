## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv.mat <- NULL	#setting initial inverse value as NULL
 set.mat <- function(y) {	
    x <<- y	        #If new value is being assigned to the matrix, inverse value is again set to Null  
    inv.mat <<- NULL	
  }	
  	
  get.mat <- function() x         #returnes stored    matrix                  
  set.inv <- function(inverse) inv.mat <<- inverse  
  get.inv <- function() inv.mat                     	
  list(set.mat = set.mat, get.mat = get.mat,	
       set.inv = set.inv, get.inv = get.inv)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Returning a matrix that is the inverse of 'x' when cached inverse is available
        inv.mat <- x$get.inv()	
        if(!is.null(inv.mat)) {                       
          message("Getting Cached Invertible Matrix")   	
          return(inv.mat)                             
        }	
          	
	## if inverse is not cached it is calculated
        matrix_data <- x$get.mat()                     
        inv.mat <- solve(matrix_data)            	
        x$set.inv(inv.mat)                         
        return(inv.mat)                               
}
