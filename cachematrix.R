##This function creates a matrix object which is a list containing a function to:
## a. Set the value of the matrix
## b. Get the value of the matrix
## c. Set the value of the inverse
## d. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
{  m <- NULL     
 set <- function(y)  
 {     
    x <<- y      
    m <<- NULL 
 }  
   get <-function() x 
   setinverse <- function(inverse) m <<- inverse  
   getinverse <- function() m  
   list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## The below function calculates the inverse of the matric created with the above function as follows:
## It first checks to see whether the inverse has been calculated
## If calculated, it gets the inverse from the cache and skips the calculation
## Else it calculates the matrix inverse and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) 
 {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()    
        if(!is.null(m))     
        {    
                message("getting cached inverse matrix")   
                return(m)     
        }     
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
