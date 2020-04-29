
# 'MakeCacheMatrix' takes a matrix as argument and recieved, when created, its own environment containing:
# 1) Two data objects: 'x' (to recieve a matrix) and 'j' (to store its inverse matrix)
# 2) a list of functions (with their own environment) that point to those two objects (in their parent environment)

# When 'MakeCacheMatrix' is given a matrix as argument and the resulting list is assigned to an object,
# that objects (that we will call 'MyMatrixObject' in the rest of the text) recieves a complete copy of the environment of 'MakeCacheMatrix'.

makeCacheMatrix <- function(x = matrix()) {
        j<<-NULL
        
        # - 'set' allows to change matrices and to reset the inverse matrix to 'NULL' without having to use 'MakeCacheMatrix' again.
        # The deep assignment arrow (<<-) modifies the variables 'x' and 'j' found in the parent 'MyMatrixObject' environment.
        # In that case, the data previously cached in the 'MyMatrixObject' is lost. To cache the inverse of several matrices,
        # we would need to run 'MakeCacheMatrix' for each one and create as many objects.
        set<-function(y){
                j<<-NULL
                x<<-y
        }
        
        
        # - 'get' allows to retrieve the matrix 'x'. The variable 'x' is absent from its environment, but due to lexical scoping,
        #  the functions is able to find it in its parent 'MyMatrixObject' environment.
        get<-function(){
                x
        }
        
        # - 'setinv' allows to cache the inverse of the matrix, after the function 'cacheSolve' has calculated it.
        # The deep assignment arrow (<<-) modifies the variable 'j' in its parent 'MyMatrixObject' environment.
        setinv<-function(i){
                j<<-i
        }
        
        # - 'getinv' retrieves the inverse of the matrix from the cache in exactly the same way as the function 'get'.
        # The variable 'j' is absent from its environment, but due to lexical scoping; the functions retrieves it from the parent environment.
        getinv<-function(){
                j
        }
        
        # 'MakeCacheMatrix'returns a list in which each element (function) is named, allowing us to call individual functions from 'cacheSolve'
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


# 'cacheSolve' takes as argument the 'MyMatrixObject' object created in the global environment by the function 'makeCacheMatrix'.
# It calls the functions contained in that object, which in turn use the variables belonging to the environment in which they were created (the 'MyMatrixObject' environment).

# - If it has not been calculated, the function checks if the matrix is invertible.
#     - If it isn't, it returns the message "This matrix is not invertible", if it is not square, a message is returned, indicating the problem.
#     - If it is invertible, the inverse is calculated, cached in the 'MyMatrixObject' environment using the 'setinv'function, and returned.

# The argument does not need to be 'x'. It is a local variable in the current environment of 'cacheSolve',
# I find it clearer to call it 'mmo' for my matrix object, which is the list of functions created in that environment.

cacheSolve <- function(mmo, ...) { 
        
        # The content of the 'j' variable in the parent environment of 'MyMatrixObject' is retrieved
        # by the 'getinv' function from the list of 'MyMatrixObject'
        # and then assigned (<-) to a variable in the current environment of the 'cacheSolve' function.
        i<-mmo$getinv()
        
        # If the inverse of the matrix has already been calculated, it is returned.
        if(!is.null(i)){
                message("getting cached inverse matrix")
                return(i) # The execution of the code ends after 'return'
        }
        
        # If the inverse matrix hasn't been computed yet, 
        # the matrix is retrieved running the function 'get' from the list of 'MyMatrixObject'.
        x<-mmo$get()
        
        # I added this code to check if the matrix is square
        d<-dim(x)
        if(d[1]!=d[2]){
                return("No inverse. The matrix should be square.")
        }
        
        # A square matrix has an inverse iff its determinant is not 0.
        if(det(x)!=0){   # After checking this,...
                i<-solve(x)   # the inverse of the matrix is computed and assigned to a variable in the current environment.
                mmo$setinv(i) # We call the 'setinv' function from the list of 'MyMatrixObject' to cache the result in its parent environment.
                return(i)     # The result is returned   
        }
        
        # If the determinant is 0, the following message is sent:
        message("This matrix is not invertible")
}


## To test the code:
##-----------------
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#mcm<-makeCacheMatrix(m1)
#cacheSolve(mcm)
#(n1<-cacheSolve(mcm))
#n1%*%m1 #as expected, when we multiply the matrix by its inverse, we get an identity matrix

## To change matrices without using makeCacheMatrix again:
##-------------------------------------------------------
#m2<-matrix(1:4,nrow=2)
#mcm$set(m2)
#cacheSolve(mcm)
#(n2<-cacheSolve(mcm))
#n2%*%m2 #as expected, when we multiply the matrix by its inverse, we get an identity matrix

## Rem: With larger matrices, there may be rounding errors with the identity matrix.
# This is due to the finite precision of R (of the order of 10^-16)
# To display the identity matrix and chech that everything is correct, we can round it to a few decimal places.