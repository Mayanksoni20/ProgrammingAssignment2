    ## The following are special function which calculate the inverse of a matrix and stores them for future use so that you don't have to calculate it again.
    
    ## makeCacheMatrix takes in a matrix, and returns a list of subfunctions to either 
    ## 1) set the value of the matrix
    ## 2) get the current value of the matrix
    ## 3) get the inverse of the matrix
    ## 4) set the inverse of the matrix
    
    makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }
    
    
    ## cacheSolve check if the inverse of a matrix has been created previously and cached. 
    ### If so, it returns the cached value. Otherwise, it computes the inverse.
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
    }
## Let's test if the code is working fine
    TM=makeCacheMatrix(matrix(c(1,0,0,1),2,2)) ##TM=Test matrix to check the code.
    cacheSolve(TM) ## calculating the inverse
    TM$getinv() ## Checking if the inverse calculated above is in the cache
    TM$setinv(5) ## setting the inverse to 5
    cacheSolve(TM)## should return 5, as I set it to 5 in the above line
    ## Bingo It does !!
    # The code works fine :) 
