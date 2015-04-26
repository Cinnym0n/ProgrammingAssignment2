## `makeCacheMatrix` and 'cacheSolve' are two functions used together to
## compute and cache the inverse of a matrix.
##
## To use them, type:
##     makeCacheMatrix(cacheSolve(x))   where 'x' is a square, intvertable,
##                                      user-specified input matrix

makeCacheMatrix <- function(matrin = matrix()) {
 ## `makeCacheMatrix`: This function creates a special "matrix" object that can
 ## cache its inverse. To do this, it does the following:
 ##     1.  set the value of the matrix
 ##     2.  get the value of the matrix
 ##     3.  set the value of the inverse matrix
 ##     4.  get the value of the inverse matrix

 ##   'matrin' is the input matrix - its inverse will be retreived or calculated

    ## Initialize matrix matr.inv to NULL
    matr.invr <- NULL

    ## create function 'set' to set 'matrin' to input matrix, initialize
    ## 'matr.invr' (output matrix) to NULL, & add both to the global environment

    ## 'matr.invr' is a matrix object to hold the inverse matrix of 'matrin'
    ## 'y' is a matrix input to this function
    ##     NOTE: '<<-' adds/replaces the preceding object in the local and
    ##           global environments
    set <- function(y) {
        matrin <<- y
        matr.invr <<- NULL
    }

    ## create function 'get' to get 'matrin' (input matrix) that already exists
    get <- function() matrin

    ## create function 'setinvr' to calculate 'matr.invr' as the inverse matrix
    ## of matrin and put/replace it in the global environmrent
    setinvr <- function(solve) matr.invr <<- solve

    ## create function 'getinvr' to retreive 'matr.invr' that already exists
    getinvr <- function() matr.invr

    ## Output of this function is the list of functions
    list(set = set, get = get,
         setinvr = setinvr,
         getinvr = getinvr)

}   #end function makeCacheMatrix


cacheSolve <- function(matrin, ...) {
 ## `cacheSolve`: This function computes the inverse of the special "matrix"
 ## created by `makeCacheMatrix` OR retrieves the previously computed and
 ## cached inverse (if the inverse already exists & the matrix hasn't changed).
 ## NOTE: This code assumes the matrix is invertable.
 ## 'solve' is a preset function that computes the inverse of a square matrix.

 ##     'matrin' is the input matrix (its inverse will be found or calculated)

    ## Retrieve the cached inverse as 'matr.inv' matrix using the 'getmean'
    ## (from makeCacheMatrix) on the input matrix 'matrin'.
    matr.invr <- matrin$getinvr()

    ## If the inverse was previously computed and cached (not NULL), return the
    ## retreived inverse 'matr.inv', and report that cached data is being used

    if(!is.null(matr.invr)) {
        message("getting cached data")
        return(matr.invr)
    }

    ## Otherwise, initialize and use:
    ## 'data' is a matrix to hold the input matrix 'matrin' retrieved using the
    ##  function 'get' (from makeCacheMatrix).
    ## 'matr.invr' is a matrix to hold the inverse of 'matrin' calculated using
    ##  the R function 'solve'.
    data <- matrix()
    data <- matrin$get()
    matr.invr <- solve(data, ...)

    ## Set the inverse (matr.invr) in the cache via the `setinvr` function (from
    ## makeCacheMatrix).
    matrin$setinvr(matr.invr)

    ## Return a matrix that is the inverse of 'matrin'
    matr.invr

}   #end function cacheSolve
