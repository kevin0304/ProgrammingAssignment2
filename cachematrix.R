# makeCacheMatrix is a function, which takes a matrix as an input.
# It inverts the matrix with the superassignment and stoes it in getsolve.
# Each time the function is called it stores the matrix in a list.


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


# cacheSolve takes a matrix made by cachematrix as input.
# it acces the object to x and gets the solve value.
# It checks if the matrix (m) has already been calculated and if yes it return it from the list
# if not it inverts the matrix and returns it.


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


