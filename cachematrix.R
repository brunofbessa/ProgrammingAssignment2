#These two functions makeCacheMAtrix and cacheSolve will calculate the inverse of a matrix
#given as argument to makeCacheMAtrix and cache this inverse matrix so thar it will be
#available if it is needed and avoid recalculation. There will be functions defined in
#makeCacheMAtrix that will passed as arguments to cacheSolve.

#_________________________________________________________________________________

#makeCacheMatrix gets a numeric matrix as argument and returns a list of functions 
#to set the values to the matrix, get the values, calculate and get the inverse matrix
makeCacheMatrix <- function( matrix_A = numeric() ) {
        #Function gets a matrix as argument as default
        cache <- NULL
        #Erasing the cache first
        setValues <- function( values ) {
                #Seting the matrix values
                matrix_A <<- values
                #Erasing the cache
                cache <<- NULL
        }
        getValues <- function() matrix_A
        #Function to get the values
        setInverse <- function(solve) cache <<- solve
        #calculating the inverse matrix
        getInverse <- function() cache
        #caching the inverse matrix
        list (setValues = setValues, 
              getValues = getValues, 
              setInverse = setInverse, 
              getInverse = getInverse
              )
        #makeCacheMatrix returns a list of this functions
}

#_______________________________________________________________________________________
#cacheSolve gets the list of functions from makeCacheMatrix as argument and tests if 
#the inverse matrix is already calculated and cached. It is able to calculate the 
#inverse using the functions from the makeCacheMatrix environment and returns the inversem matrix
cacheSolve <- function( matrix_A, ... ) {
        #Function gets a list of functions created by the previous makeCacheMatrix function
        matrix_Inv <- matrix_A$getInverse( )
        #Accessing getInverse( ) from the environment of makeCacheMatrix()
        if ( !is.null( matrix_Inv ) ) {
                message( "Getting cached data" )
                return( matrix_Inv )
        }
        #Testing if the cache is already filled with the inverse matrix
        data <- matrix_A$getValues( )
        matrix_Inv <- solve( data, ... )
        matrix_A$setInverse( matrix_Inv )
        #geting the data and the tools (functions) to calculate the inverse matrix from makeCacheMatrix()
        matrix_Inv
        #Returning the inverse matrix
}
