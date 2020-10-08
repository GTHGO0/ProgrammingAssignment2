## MakeCacheMatrix ASSIGNMENT 2
## by Hans Gonzalez 07-10-2020
## created for the "Programming Assignment 2: Lexical Scoping
## as part of the course "R programming"
## an Online course in Coursera.com.

## makeCacheMatrix, stores a matrix and assign 4 sub-functions (edit, set, set inverse and get inverse)
## cacheSolve, calculates the inverse of a matrix created with makeCacheMatrix.

## Write a short comment describing this function

## DESCRIPTION OF THE FUNCTION makeCacheMatrix:
##   This Function allows you to create a matrix as an object,
##   the function will create a list of 4 internal sub-functions.
##   (1) $setmtrx,     is a function to set new values in "x" as a new matrix
##   (2) $getmtrx,     is a function that retrieves the values stored in the matrix
##   (3) $setInverse,  is a function that allows you to set "any" value,
##                     as the Inverse of the matrix (is not a math calculation)
##   (4) $getInverse,  is a function that retrieves the inverse of a Matrix.
##                     if the Inverse was previously calculated, the function
##                     keeps it in memory and retrieves it form the cache.
##                     otherwise it will display the value as "NULL".

makeCacheMatrix <- function(values = numeric(), nrow, ncol) {
          # values = can be a numeric vector as = C(n1,n2,n3,n...) or range as 1:4
          # nrow = the number of rows in your matrix
          # ncol = the number of columns in your matrix
          # x = a variable to store the values of a matrix object
          # I = contains the "Inverse of a Matrix"
  
  x <- matrix(values, nrow, ncol)           # stores in "x" the matrix as an object
  I <- NULL                                 # sets as Null the Inverse of "x" matrix
  
  setmtrx <- function(values, nrow, ncol) { # (1) $setmtrx, is a function to set new values in "x" as a new matrix
    y <- matrix(values, nrow, ncol)         #     stores in "y" the values of the new matrix
    x <<- y                                 #     resets the values in "x" with the values of "y"
    I <<- NULL                              #     sets the Inverse of a matrix with "NULL"
  }
  getmtrx <- function() x                   # (2) $getmtrx, is a function that retrieves the values stored "x" 
  setInverse <- function(solve) I <<- solve # (3) $setInverse is a function that allows you to set "any" value as the inverse
                                            #     of "x". It is not a math calculation, it can be, for example a vector in the form = c(1,2,3,4)  
  getInverse <- function() I                # (4) $getInverse, retrieves the inverse of a matrix if its cached, otherwise returns "NULL"
  list(setmtrx = setmtrx, getmtrx = getmtrx,#     this is a list to store the functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## DESCRIPTION OF THE FUNCTION cacheSolve:
##   This Function allows you to calculate and caches the Inverse of a Matrix.
##   If the Inverse of a matrix was previously set or calculated, then the function
##   will retrieve the result from the cache, otherwise the function will perform
##   the calculation using the Solve() function.

cacheSolve <- function(x, ...) {
  # I = a variable that stores the Inverse of a Matrix
  I <- x$getInverse()                  #  retrieves from the cache any value previously calculated
  if(!is.null(I)) {                    #  if the value of "I" is different from "Null" then...
    message("getting cached data") 
    return(I)                          #  ...the function returns the previous value stored in the cache.
  }
  data <- x$getmtrx()                  #  else, the function stores in "data" the values of the matrix
  I <- solve(data, ...)                #  then applies the "solve()" function to calculate the inverse of a matrix
                                       #  and stores it in the variable "I"
  x$setInverse(I)                      #  assigns to x$setInverse(I) the value of this new calculation.
  I                                    #  prints the "invert of a matrix"
}
