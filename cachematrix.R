##The cachematrix.R file contains two functions, makeCacheMatrix() and cachemean(). 
##The first function in the file, makeCacheMatrix() creates an R object that stores a matrix and its inverse. 
##The second function, cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to 
##retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.

##--------------------------------
##makeCacheMatrix() builds a set of functions and returns the functions within a list to the 
##parent environment. The global environment contains the makeCacheMatrix() environment. 
##All the other functions is present in the makeCacheMatrix() environment.

#myMatrix contains pointers to functions that are within the makeCacheMatrix() environment 
##after the function ends, so these pointers prevent the memory consumed by makeCacheMatrix() 
##from being released by the garbage collector. Therefore, the entire makeCacheMatrix() 
##environment stays in memory, and myMatrix can access its functions as well as any data 
##in that environment that is referenced in its functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##---------------------------
##Without cacheSolve(), the makeCacheMatrix() function is incomplete. cacheSolve() is required to 
##populate or retrieve the inverse by the solve function from a mtrix from makeCacheMatrix().
##We take advantage of lexical scoping and the fact that functions that return objects 
##of type list() also allow access to any other objects defined in the environment of the original function. 
##In the specific instance of makeCacheMatrix() this means that subsequent code can access the values 
##of x or m through the use of getters and setters. This is how cachemean() is able to calculate and 
##store the inverse for the input argument if it is of type makeCacheMatrix(). 
##Because list elements in makeCacheMatrix() are defined with names, we can access these functions 
##with the $ form of the extract operator.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
