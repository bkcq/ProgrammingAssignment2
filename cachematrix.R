################################################################################ 
## A pair of functions that will take a square matrix and store it's          ##
## inverse to prevent wasting compute time of already calculated data         ##
## Assumes the square matrix HAS an inverse (is solvable - no error code).    ##
## (my thoughts - this type of code creates an object and defines properties  ##
## and methods for it (getter and setter methods, and start matrix/inverse   ##
##  matrix properties))                                                       ##
################################################################################

## the makeCacheMatrix function creates an 'object' with get and set methods
## and stores the matrix and it's inverse in properties (ms {matrix start} 
## and im {inverse matrix})

makeCacheMatrix <- function(ms = matrix()) {
    im<-NULL
    ## create the objects outside this scope
    set <- function(y) {
        ms<<-y
        im<<-NULL
    }
    ## function to retrieve the starting matrix  
    get <- function() ms
    ## store the inverse matrix
    setInvMtrx<-function(solve) im<<-solve
    ## return the inverse matrix
    getInvMtrx<- function() im
   
   #makes object/properties accessible from the parent scope
   list(set=set,get=get, setInvMtrx=setInvMtrx, getInvMtrx=getInvMtrx)
   
}


## Code that checks if the matrix under consideration has already been stored
## and if so returns the stored inverse matrix.  Otherwise it calculates 

cacheSolve <- function(ms, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## IS a inversematrix computed?
    ## get stored inverted matrix ** note it was NULL if undefined**
    im<-ms$getInvMtrx()
   
    if (!is.null(im))  {
        ## since already solved exit with stored inverse
        return(im)
    }
    ## solve and store inverse of passed matrix
    stuff<-ms$get()
    im<-solve(stuff, ...)
    ##store the inverse
    ms$setInvMtrx(im)
    ##last line is returned
    im
    
    
}
