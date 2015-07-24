## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #create a variable "inv" defined inside the scope of the main function
    inv <- NULL
    set <- function(y){
      #set the value of "x" in the scope of the main function (from the argument)
      x <<- y
      #reset the value of "inv" in the scope of the main function because there is a new matrix
      inv <<- NULL
    }
    #get the value of "x" from the scope of the main function
    get <- function() x
    #set the value of "inv" in the scope of the main function according to input
    setinv <- function(inverse) inv <<- inverse
    #return the value of "inv" in the scope of the main function
    getinv <- function() inv
    #return the list containing the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## return the inverse matrix from the cached data if present, otherwise calculate and cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the value of the inv using getinv function from list x.
        inv <- x$getinv()
        #check if "inv" is null, if not return inv, otherwise calculate inverse
        if(!is.null(inv)){
          message("getting cached data")
          #return inv and exit the function
          return(inv)
        }
        #get matrix data
        data <- x$get()
        #calculate the inverse matrix
        inv <- solve(data)
        #set the inverse matrix
        x$setinv(inv)
        #return inv
        inv
}
