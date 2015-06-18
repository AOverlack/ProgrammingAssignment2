
## This function performs initial inversion calculation of a matrix and persistent value storage
## for input data and returned value so that inversion calculation can be limited
## to first input or change of input data only. Calculated value of inversion is retrieved if present.
## It assumes that the input matrix is invertible. It must be run prior to executing
## the next module "cacheSolve", to generate the store and retrieve functions in the list

makeCacheMatrix  <- function(x){
        inv <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## This part of the function retrieves the stored value of the inverted matrix if stored previously
## If the inverse matrix is already present, it returns it,
## otherwise it calculates and stores it and then returns it.

cacheSolve <- function(x) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
