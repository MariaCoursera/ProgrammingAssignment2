# setwd ("C:/Users/Owner/Desktop/Coursera/Data/assn_2")
# source "cachematrix.R"
# options(digits=4)

## The purpose of Assignment2 is to take advantage of the 
## scoping rules of the R language and how they can be 
## manipulated to preserve state inside of an R object.
##
## We'll use the <<- operator  
## to assign a value to an object 
## in an environment that is different 
## from the current environment
##
##
##-----------------------------------------------------
## MakeCacheMatrix is a function that 
## creates a special "vector", which is really a list 
## of functions  
## 1. set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the value of the mean
## 4.	get the value of the mean
#  
#  Assignment 2 additions
#  5. set the value of the matrix inverse
#  6. get the value of the matrix inverse
##--------------------------------------------------------
## Assignment 2:-
##    This function creates a special "matrix" object 
##    that can cache its inverse.
##---------------------------------------------------------

##-----------------------------------------------------
  
  #------------------------------------------------------------
  # 1. Create new closure functions (get, set, etc.), 
  #    note: closure functions cause R to automatically 
  #          preserve the associated parent environment
  # 2. return handle to associated parent environment
  #-------------------------------------------------------------
  ## The makeCacheMatrix function contains closures.  
  #  Closures are functions written by another function.
  #  Closures "enclcose" the environment of the parent function..
  #  Closure functions:  get, set, etc.
  #
  # Parent function: makeCacheMatrix
  ## Variables in environment of the parent function: 
  #  x: input matrix from parent function header
  #  m: variable used in parent
  #  x_inverse: variable used in parent

makeCacheMatrix <- function(x = matrix()) {
  
  #------------------------------------------------------
  # We're creating a new "parent environment" that contains
  # the cached variables x, m, x_inverse.   
  # x contains the value of the matrix passed in as an argument.
  # m and x_inverse are initialized to NULL.  
  #-----------------------------------------------------
  m <- NULL  
  x_inverse <- NULL
  
  
  #-------------------------------------------------------------
  # Define closure function: set
  # For the specified environment, 
  # change the value of the "cached matrix",
  # set the mean and inverse to NULL so we know the 
  # matrix has changed.
  #--------------------------------------------------------------
  set <- function(y){
      x <<- y     # set the "cached matriX" to the matrix "y"
      m <<- NULL  # set the "cached mean" to NULL 
      x_inverse <<- NULL # set the "cached inverse"to NULL
      message ("Changing cached Matrix: setting mean and inverse to NULL")
  }
  
  #---------------------------------
  # Define closure function: get
  # For the specified environment, return the cached matrix
  #---------------------------------  
  get <- function() x   
  
  #---------------------------------
  # Define closure function: setmean 
  # For the specified environment, change value of the cached mean
  #---------------------------------
  setmean <- function(mean) m <<- mean 
  
  #---------------------------------
  # Define closure function: getmean
  # For the specified environment, return value of the cached mean
  # Note: NULL means the inverse has not been calculated (it's also NULL)
  #---------------------------------
  getmean <- function() m    # return value of cached_mean
  
  
  #---------------------------------
  # Define closure function: set_inverse 
  #---------------------------------
  set_inverse <- function(inverse=matrix()) x_inverse <<- inverse # set value of cached_mean to value of mean
  
  #---------------------------------
  # Define closure function: get_inverse
  #---------------------------------  
  get_inverse <- function() x_inverse  
  
  #-----------------------------------------------------------
  # return "handle" to this new cached parent environment
  #      
  # The environment includes a special "vector" which is 
  # list of functions to access
  # the cached data  (x aka matrix, m aka mean, x_inverse)
  #
  # access cached data via function call syntax:
  #                     handle$set(matrix), handle$get(), 
  #                     handle$setmean(mean), handle$getmean()
  #                     handle$set_inverse(matrix), handle$get_inverse()
  #-----------------------------------------------------------
  list(set = set, get = get,
         setmean = setmean,
         getmean = getmean,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}
  


##-----------------------------------------------------
## The following function calculates the mean of the 
## special "vector" created with the above function. 
## 1) check to see if the mean has already been calculated. 
#     a) If yes, get the mean from the cache 
#     b) Otherwise, calculate the mean of the data 
#        and set the value of the mean in the cache via 
#        the setmean function.
#
# 2) for Assignment2, return a matrix that is the 
#    inverse of the cached_matrix 'x'
##-----------------------------------------------------
cacheSolve <- function(x, ...) {
  
  #----------------------------------------
  # Check what's in the cache for the 
  # requested environment 'x'
  #----------------------------------------         
  the_inverse <- x$get_inverse()
  the_mean <- x$getmean() 
  
  
  #---------------------------------------------------------
  # If both mean and inverse are not null, 
  # just return the inverse
  # Note: both mean and inverse are set to NULL when the 
  #       matrix is changed via the env$set(new_matrix) function
  #---------------------------------------------------------
  if(!is.null(the_mean) & !is.null(the_inverse)) {
    message("getting cached data")
    return (the_inverse)   
  }
  

  
  message ("calculating inverse")  
  data <- x$get()            # get cached_matrix
  the_inverse <- solve(data)
  if (!is.matrix(the_inverse)) {
    the_inverse <- NULL
    return (the_inverse)
  }    
  
  message ("Setting mean and inverse")
  x$set_inverse(the_inverse)
  the_mean <- mean(data, ...)  # call R function to calculate mean
  x$setmean(the_mean) 
  
  the_inverse   # return inverse of matrix
}


