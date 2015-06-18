###################################################################################
# makeCacheMatrix() - 
# ~~~~~~~~~~~~~~~~~
# Function purpose:
#  @ To create a list object containing 4 sub-functions which can be used to :
#
#       a) store the current input source matrix to memory and initialize a new empty 
#	   cache holder for storing the inverted matrix in memory :-> set() sub-function
#
#	b) retrieve and display the input source matrix  :-> get() sub-function
#
#	c) set/storing the inverted matrix into cache holder  :-> setInverse() sub-function	 
#
#	d) retrieve and display the stored inverted matrix from cache holder :-> getInverse() sub-function
#
#       *Sub-functions are generated through makeCacheMatrix main function call.
#
#	Sample calling method: 
#	     >  mat_a<-makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
#
#####################################################################################


makeCacheMatrix <- function(x = matrix()) {
        
        ## Input parameter: x being an matrix object 
        
        ## initialize cache holder for storing the inverted matrix
        i <- NULL 
        
        ## function to set current input source matrix and initialize holder for inverted matrix	
        set <- function(y) {
                ## store input source matrix into calling environment 
                x <<- y 
                
                ## re-initialize inverted matrix holder from calling environment
                ## especially needed if set() sub-function is called directly 
                ## to change the original input source matrix then the inverted matrix should be 
                ## re-initilized to ensure consistent pairing result
                i <<- NULL 
        }
        ## function to retrieve and display the input source matrix
        get <- function() x
        
        ## function to set/storing the inverted matrix into cache holder 
        setInverse <- function(inv) i <<- inv
        
        ## function to retrieve and display the stored inverted matrix
        getInverse <- function() i
        
        ## Set the sub-function definitions into a list and return as a list object
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

###############################################################################
# cacheSolve() -
# ~~~~~~~~~~~~~~~
# Function purpose:
# @
#	
#  a) To invert an input source matrix object if it has not been inverted before and store it into memory holder		  
#
#  b) Else if an inverted matrix of the source input matrix exists, retrieve it from cache and avoid
#     re-doing invertion again
# 
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
        
        ## Input parameter: x being an processed cacheMatrix object
        
        ## attempt to retrieve inverted matrix from cache of input cacheMatrix object
        i <- x$getInverse()
        
        ## if previous inverted matrix do exist, print message, display result and exit function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## if no previous stored inverted matrix found, retrieve the input source matrix through  
        ## the input cacheMatrix object's sub-function
        data <- x$get()
        
        ## Invert the retrieved input source matrix
        i <- solve(data, ...)
        
        ## store inverted matrix into cache, make use of input cacheMatrix object's sub-function
        x$setInverse(i)
        
        ## display the inverted matrix and exit function
        i
}


###############################################################################
# Additional note: 
# Testing approach and verify result of each execution
#	> mat_a<-makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
#	> mat_a
#	> summary(mat_a)
#	> mat_a$get()
#	> mat_a$getInverse()
#	> cacheSolve(mat_a)
#	> mat_a$getInverse()
#	> cacheSolve(mat_a)	  
# 	> mat_a$set(matrix(c(6,7,9,2), nrow=2, ncol=2))
#       > mat_a$get()
#       > mat_a$getInverse()
#	> cacheSolve(mat_a)
#	> cacheSolve(mat_a)
###############################################################################