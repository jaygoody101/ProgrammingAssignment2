## Pair of functions to invert a matrix and cache the result for subsequent use
## Using example "mean" function as a template:
## change vectors to matrices where appropriate
## change "NULL" setting to "NaN"
## change evaluate call from Mean to Solve

## Creates list of functions to store cached version of a matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
        mInv <- as.matrix(NaN)
        set <- function(y) {
                x <<- y
                mInv <<- as.matrix(NaN)
        }
        get <- function() x
        setInv <- function(pInv) mInv <<- pInv
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Checks for existence of cached matrix inverse.
## If present, uses it; if not, inverts input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        if(!is.nan(mInv[1,1])) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        mInv <-as.matrix(x$setInv(mInv))
        mInv
        
        }
