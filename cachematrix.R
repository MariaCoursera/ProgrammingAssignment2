# setwd ("C:/Users/Owner/Desktop/Coursera/Data/assn_2")
# source "cachematrix.R"
# options(digits=4)

  
#-----------------------------------------------------
## MakeCacheMatrix
## Input: matrix
## Output: vector (aka handle)  
##         containing functions that provide access to the 
##         "cached variables" x, m, x_inverse
##         that are associated with the 
#          cached "parent environment" 
##        
#  Notes: 
#  - The cached "parent environment" is created by defining
#    nested or "closure" functions (get, set, etc.). 
#    closure" functions cause R to automatically 
#    preserve (cache) the associated "parent environment".  
# - Must use the get/set functions to access variables in the 
#   specified cached parent environment.
# - The set functions use the <<- operator to 
#   write to variables in the cached parent environment.
#-------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  
  #------------------------------------------------------
  # We're creating a new "parent environment" that contains
  # the cached variables x, m, x_inverse.   
  # x contains the value of the matrix passed in as an argument.
  # m and x_inverse are initialized to NULL.  
  #-----------------------------------------------------
  m <- NULL             # set the mean to NULL
  x_inverse <- NULL     # set the inverse to NULL
  
  
  #-------------------------------------------------------------
  # function: set
  # Purpose: Set cached matrix
  # Details: For the specified environment, 
  # assign a new value to the "cached matrix" variable x,
  # set the mean and inverse to NULL so we know the 
  # matrix has changed.
  # Note: use <<- operator to assign values to variables in 
  #       "parent environment"
  #--------------------------------------------------------------
  set <- function(y){
      x <<- y             # set the "cached matriX" to the matrix "y"
      m <<- NULL          # set the "cached mean" to NULL 
      x_inverse <<- NULL  # set the "cached inverse"to NULL
      message ("Changing cached Matrix: setting mean and inverse to NULL")
  }
  
  #-----------------------------------------------------------
  # function: get
  # Purpose: Get cached matrix
  # Details: For the specified environment, 
  #          return the cached matrix
  #-----------------------------------------------------------  
  get <- function() x   
  
  #-----------------------------------------------------------
  # function: setmean 
  # Details: For the specified environment, set value of the cached mean
  #------------------------------------------------------------
  setmean <- function(mean) m <<- mean 
  
  #----------------------------------------------------------
  # function: getmean
  # Details: For the specified environment, return value of the cached mean
  # Note: NULL implies the inverse has not been calculated 
  #       (the inverse is most likely also NULL, but it's not guaranteed to be NULL)
  #----------------------------------------------------------
  getmean <- function() m   
  
  #---------------------------------
  # function: set_inverse 
  # Details: For the specified environment, set value of the cached inverse
  #---------------------------------
  set_inverse <- function(inverse=matrix()) x_inverse <<- inverse 
  
  #---------------------------------
  # function: get_inverse
  # Details: For the specified environment, return value of the cached inverse
  #---------------------------------  
  get_inverse <- function() x_inverse  
  
  #-----------------------------------------------------------
  # return parent environment "function vector"
  #      
  # Note: Syntax to access cached data is: 
  #                     env1 = makeCachedMatrix (TestMatrix1)
  #                     env1$get(), env1$getmean(), etc.
  #                     env1$set(aDifferentMatrix), etc.
  #-----------------------------------------------------------
  list(set = set, get = get,
         setmean = setmean,
         getmean = getmean,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}
  


##-----------------------------------------------------
## function cacheSolve
## Input: "function vector" to a cached parent environment
## Output: Inverse matrix.  
##         Note: a NULL matrix will be returned if the inverse matrix can 
##        not be calculated due to errors (matrix isn't square, other errors, etc.)
##
## Details: 
## - If the cached inverse has already been calculated, return the cached inverse 
## - Otherwise, calculate both the inverse and the mean, and store these values in the cache
## - Error Checking: If the inverse cannot be calculated, set the inverse to NULL
##------------------------------------------------------------------------------------
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
  #       matrix is first assigned via "env1 <- makeCacheMatrix(original_matrix)", or 
  #       the matrix is changed via the "env1$set(new_matrix)" 
  #---------------------------------------------------------
  if(!is.null(the_mean) & !is.null(the_inverse)) {
    message("returning cached inverse")
    return (the_inverse)   
  }
  

  #-------------------------------------------------------------
  # The matrix has changed, so calculate inverse
  #-------------------------------------------------------------
  message ("calculating inverse")  
  data <- x$get()                     # get cached_matrix
  the_inverse <- solve(data)          # calculate inverse
  if (!is.matrix(the_inverse)) {      # check if solve returned an error
    the_inverse <- NULL
    x$set_inverse(the_inverse)        # set cached inverse to NULL
    return (the_inverse)              # return NULL matrix
  }    
  
  message ("Setting mean and inverse")
  x$set_inverse(the_inverse)          # set cached inverse
  the_mean <- mean(data, ...)         # calculate mean
  x$setmean(the_mean)                 # set cached mean
  
  the_inverse   # return inverse of matrix
}



  
  ##-------------------------------------------------------------------------
  ## More comments and explanation...
  ##
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
  #--------------------------------------------------------------------------