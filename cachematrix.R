## By analogy with vector example, let's create a function 
## "makeCacheMatrix" that stores functions:
## - to set values for matrix in global environment
## - to retrieve the matrix
## - to store a value for inverted matrix
## - to retrieve the inverted matrix

makeCacheMatrix <- function(x = matrix()) {  ## class of the object will be matrix
        s <- NULL       ## s will stand for solved matrix, it will default to
        ## NULL because nothing has been solved yet
        
        ## function 1: setting the values for matrix in global environment
        ## (in case we want to change our stored matrix)
        set <- function(y) {
                x <<- y     ## "x" globally will become "y", i.e. our input in this func
                s <<- NULL  ## "s" globally will be reset to NULL,
                ## because inverse was not calculated for the new matrix 
        }
        
        ## function 2: retrieving the stored values for x        
        get <- function() x
        
        ## function 3: setting the values for inverted matrix in global environment ("s")
        setinverse <- function(solve_inverse) s <<- solve_inverse
        
        ## function 4: retrieving the stored values for s        
        getinverse <- function() s
        
        ## storing our 4 functions in "makeMatrix" as a list, so it can be assigned to an object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## END of part 1



## Now, let's make a second function that checks if there is already a calculation
## of inverted matrix for the object from "makeCacheMatrix" and if not, 
## calculates the inverted matrix

cacheSolve <- function(x) {
        s <- x$getinverse()     ## "s" in this function retrieves inverted matrix from 
        ## object "x" created with "makeCacheMatrix"
        
        if(!is.null(s)) {       ## checks if stored vallue is not NULL
                
                message("getting cached data")  ## if it is, writes a message to let 
                ## users know that data is from cache
                return(s)                       ## and returns solved inverted matrix
                ## thus ending the function
        }
        
        data <- x$get()                         ## if the above check is FALSE
        ## then retrieves the matrix from "x"
        
        s <- solve(data)                   ## then solve the matrix for its inverse
        
        x$setinverse(s)                         ## writes the result "s" to cache 
        ## in our created object 
        
        s                                       ## returns the inverted matrix
}

## THE END