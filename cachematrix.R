## My functions allow you to pass a matrix to a function, and then save the
#inverse of that function to the cache, so it does not have to be repeatedly solved

## This function takes a matrix as an argument and then store that in a list
#along with the functions that allows it to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        
        getinverse <- function(){
                inv
        }
                
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
        
        
}


# This function will find the inverse of a matrix made with MakeCaheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if (!is.null(inv)){
                message("Using data from cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}

