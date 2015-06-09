###############################################################################
# Function purpose:
#  @ To create a list containing 4 sub-functions which can be used to :
#
#        a) store the current input source matrix to memory and initialize a new empty 
#           cache holder for storing the inverted matrix in memory : -set sub-function
#
#	b) retrieve and display the input source matrix  :-get sub-function
#
#	c) set/storing the inverted matrix into cache holder  :- setInverse sub-function	 
#
#	d) retrieve and display the stored inverted matrix from cache holder :- getInverse sub-function
#
#       *Sub-functions are generated through makeCacheMatrix main function call.
#
#	Sample calling method: 
#	     >  mat_a<-makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
#
#####################################################################################


makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize cache holder for storing the inverted matrix
        i <- NULL 
        
        ## function to set current input source matrix and initialize holder for inverted matrix	
        set <- function(y) {
                ## store input source matrix into calling environment 
                x <<- y 
                
                ## re-initialize inverted matrix holder from calling environment
                ## especially useful if set() sub-function is called directly 
                ## to change the input source matrix
                i <<- NULL 
        }
        ## function to retrieve and display the input source matrix
        get <- function() x
        
        ## function to set/storing the inverted matrix into cache holder 
        setInverse <- function(inv) i <<- inv
        
        ## function to retrieve and display the stored inverted matrix
        getInverse <- function() i
        
        ## Setting the sub-function definitions into a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

###############################################################################
# Function purpose:
# @	  
#  a) To attempt retrieval of inverted matrix from cache if previously stored and bypass 
#     re-inverting of input source matrix again 	
#
#  b) To invert the input source matrix and store it into memory holder
#	
#  Consideration/Limitations:
#  Matrix inversion will only work under following conditions:
#	- input matrix is a "square" (same number of rows and columns)
#	- input matrix is not singular (determinant is zero; e.g.: singular matrix(3,6,4,8)	
#
#	Sample calling method: 
#	    >  cacheSolve(mat_a)
#
###############################################################################

cacheSolve <- function(x, ...) {
        
        ## attempt to retrieve inverted matrix from cache
        i <- x$getInverse()
        
        ## if previous inverted matrix exist, print notice, display result and exit function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## if no stored inverted matrix found, retrieve the input source matrix 
        data <- x$get()
        
        ## Invert the retrieved input source matrix
        i <- solve(data, ...)
        
        ## store inverted matrix into cache, make use of 
        x$setInverse(i)
        
        ## display the inverted matrix and exit function
        i
}

