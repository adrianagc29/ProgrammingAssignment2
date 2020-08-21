## This functions pretend to reduce the computational cost of computing
## repeatedly the inverse of a matrix. Instead, these functions allow 
## the process take place in two steps. 
##

## 
makeCacheMatrix <- function(x = matrix()) {
        
## Firstly, the makeCacheMatrix function pretends to create a matrix, also
## calculated and stores the inverse of that matrix lization of the variable that will store the inverse matrix       
        matr<-NULL
        
        #The set function assign the the values from the parent environment
        #If there is already a matrix in matr it will clear it, in order 
        #recalculate the inverse.This is done when the initial matrix is change.
        
        set <- function(x2) {
                x <<- x2
                matr <<- NULL
        }
        
        #This function returns the initial matrix (x) 
        get <- function() x
        #This function assign the solution of the inverse matrix to matr
        #which is in the parental environment
        setInverse <- function(solve) matr <<- solve
        #This function returns the calculated inverse matrix (matr)
        getInverse <- function() matr
        
        #finally, the function returns an object with the other nested
        #functions  and makes easier to determine the inverse 
        #of the matrix
        #the list is set as the name function equals the value (determine above)
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Firstly, the cacheSolve function pretends to create a matrix, also
## calculated and stores the inverse of the matrix of the MakeCacheMatrix
## function. The function has an object as main argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', which is solve
        ##with the function MakeCacheMAtrix
        matr <- x$getInverse()
        
        ##If the inverse matrix is not empty the function return a message
        if(!is.null(matr)) {
                message("getting cached data")
                return(matr)
        }
        ##When the matrix is empty, the function get the data and solve
        ##the matrix 
        data <- x$get()
        matr <- solve(data, ...)
        x$setInverse(matr)
        matr
}
