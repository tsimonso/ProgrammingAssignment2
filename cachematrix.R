
# AN EMPTY TEMPLATE : THE FUNCTION 'MakeCacheMatrix' IS CREATED
# -------------------------------------------------------------
# The function 'MakeCacheMatrix' is created in the global environment, it contains the template for a storage environment with...
# 1) Two data objects: 'x' (to recieve a matrix) and 'j' (to store its inverse matrix)
# 2) a list of functions (with their own environment) that point to those two objects (in their parent environment)
# At this point, all this is just an empty template that the function will copy every time it is run with a matrix argument.

# A SPECIFIC STORAGE ENVIRONMENT FOR OUR MATRIX : THE FUNCTION 'MakeCacheMatrix' IS RUN
#--------------------------------------------------------------------------------------
# When 'MakeCacheMatrix' is run, with a matrix argument, it creates a complete copy of its template environment for that specific matrix.
# The output is a list of functions pointing to the places in that new environment where the matrix is stored ('x')
# and where the inverse matrix will be cached ('j').
# That output can be assigned to a variable name in the global environment (e.g. 'MyMatrixObject') and later fed to the 'cacheSolve' function.

makeCacheMatrix <- function(x = matrix()) { # The function takes a matrix argument. The default value is an empty 1x1 matrix.
        j<-NULL # 'j' is set to 'NULL'. The regular assignment arrow(<-) creates a variable in the current environment of 'makeCacheMatrix'.
        
        # - 'set' recycles our storage place for another matrix.
        # It allows to change matrices and to reset the inverse matrix to 'NULL' without having to use 'MakeCacheMatrix' again.
        # The deep assignment arrows (<<-) modify the variables 'x' and 'j' in the parent environment (i.e. the 'MakeCacheMatrix' environment).
        # The data previously cached in the variables 'x' and 'j' is lost.
        # To cache the inverse of several matrices, we would need to run 'MakeCacheMatrix' for each one of them separately.
        set<-function(y){
                j<<-NULL
                x<<-y
        }
        
        # - 'get' allows to retrieve the matrix 'x'. The variable 'x' is absent from its environment, but due to lexical scoping,
        #  the function is able to find it in its parent environment (i.e. the 'MyMatrixObject' environment).
        get<-function(){
                x
        }
        
        # - 'setinv' allows to cache the inverse of the matrix, after the function 'cacheSolve' has calculated it.
        # Again, thanks to lexical scoping, 'setinv' is able to find 'j' in its parent environment and modify it using the deep assignment arrow (<<-).
        setinv<-function(i){
                j<<-i
        }
        
        # - 'getinv' retrieves the cached inverse matrix in exactly the same way as the 'get' function.
        getinv<-function(){
                j
        }
        
        # 'makeCacheMatrix'returns a list in which each element (each function) is named, allowing us to call individual functions from 'cacheSolve'
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

# ACCESSING THE STORAGE ENVIROMENT OF THE MATRIX AND COMPUTING THE INVERSE MATRIX : the 'cacheSolve' function
# -----------------------------------------------------------------------------------------------------------
# The argument of 'cacheSolve' is the list of functions created by the 'makeCacheMatrix' function in its specific 'myMatrixObject' environment.
# 'cacheSolve' calls the functions contained in that list, which in turn look for the necessary variables following the rules of lexical scoping.

# If an inverse matrix exists but has not been calculated yet, it is computed, stored and returned,
# otherwise, the cached inverse matrix already computed is retrieved.

# In the code below, the argument of 'cacheSolve' does not need to be called 'x'. It is a local variable in the current environment of 'cacheSolve'.
# I find it clearer to call it 'mmo' for 'my matrix object'.

cacheSolve <- function(mmo, ...) { 
        
        # The content of the 'j' variable is retrieved by the 'mmo$getinv' function from its parent 'myMatrixObject' environment.
        # and then assigned (<-) to a variable 'i' in the current environment of the 'cacheSolve' function.
        i<-mmo$getinv()
        
        # If the inverse of the matrix has already been calculated, it is returned.
        if(!is.null(i)){
                message("getting cached inverse matrix")
                return(i) # The execution of the code ends after 'return'
        }
        
        # If the inverse matrix hasn't been computed yet, the function 'mmo$get' is called and retrieves the matrix from its parent environment.
        x<-mmo$get()
        i<-solve(x)   # The inverse of the matrix is computed and assigned to a variable 'i' in the current environment of 'cacheSolve'.
        mmo$setinv(i) # We call the 'setinv' function to cache the result in its parent 'myMatrixObject' environment.
        i     # The result is returned.
}
