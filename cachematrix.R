##
## makeCacheMatrix : 
##
## This function takes a matrix as an input argument
## and by default an empty matrix. The inverse object is set by default
## to NULL. The <<set>> function takes value as its arguments and sets
## x equal to the argument, where x is an object in the enviroment of
## makeCacheMatrix. This is accomplished by using the assignment operator
## <<-. The same happens to inverse, where its set to NULL. This is necessary
## when you set x to a new matrix. The <<get>> function returns the value of x.
## The <<setinverse>> sets an value for the inverse object by using the <<-
## operator and assigning inverse to the argument in the setinverse function.
## The <<getinverse>> function returns the value of inverse.
## The output of the function is s list where the elements of the list are the
## functions described above. This makes it able to use the functions, f.ex. 
## if g is a function, and this function is in a list, then we can use that
## function from that list by name_of_list$name_of_element(args) where args
## are the arguments for g. This makes it able to store the inverse and its matrix
## in an object given by makeCacheMatrix without having to recalculate it. 
## 
## cacheSolve :
##
## This function takes a list that is returned from makecacheMatrix and returns
## the inverse of the matrix, which is the argument in makeCacheMatrix OR the value
## which x has been set by with the <<set>> function. The first line retrives the
## the inverse of the matrix. If the inverse is not set to NULL, then the inverse
## is returned. If its NULL, then the inverse is calculated using the solve function.
## This function solves a set of linear equations if an additional numeric vector argument 
## is passed in, but if ommitted, then the function finds the inverse of the matrix.
## The ellipsis in the cacheSolve function is also passed to the solve function.
## ----------------------------------------------------------------------------------------
##
## This function returns a list of functions that can be used to retrive or set values
## for a matrix or its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
        
        if(!is.matrix(x)) {
                return("class of input is not matrix, exiting function!")
        }
        
        inverse <- NULL
        
        set <- function(value) {
                x <<- value
                inverse <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse_input) {
                inverse <<- inverse_input
        }
        
        getinverse <- function() {
                inverse
        }
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is used to solve the inverse of the matrix and store it
## in argument that is passed into cacheSolve. If the
## inverse is not equal to NULL, it will be retrieved as the value
## given by the getinverse() function.

cacheSolve <- function(x, ...) {
        
        inverse_matrix <- x$getinverse()
        
        if(!is.null(inverse_matrix)) {
                
                print("returning cached inverse!")
                
                inverse <- inverse_matrix
        }
        
        else {
                print("calculating new inverse!")
                
                matrix_x <- x$get()
                
                inverse <- solve(matrix_x, ...)
                
                x$setinverse(inverse)
        }
        
        inverse
}
