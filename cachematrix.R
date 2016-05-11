

## This function creates an object list of four functions based on original matrix:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## creation of the object
  
        
        ## definition of the cache m
        m <- NULL
        ## setter
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the higher parent environment
                m <<- NULL ## re-initialize m in the parent environment to null
        }
        
        ##getter
        get <- function() x ## return the matrix x
        
        ##setter inverse
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        
        ## getter inverse
        getinverse <- function() m ## return the cached inverse of x
        
        ## deliver the the list of four functions:
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special object "matrix" created with the makeCacheMatrix function.

## The first logical test  hecks to see if the inverse has already been caclulated. 
##  If yes -> we apply the getter 
##  If no -> we calculate the inverse and  launch the setinverser function in order to keep this information on the background.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the special object x created by the makeCacheMatrix function.
        
        #get the inverse
        m <- x$getinverse()
        
        # logical test
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # calculation 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## Exemple of use 
# a<- makeCacheMatrix()
# class(a)
# class(a$set)  
# a$set(rbind(c(1, -1/4), c(-1/4, 1)) ) 
# a$get() 
# cacheSolve(a)



