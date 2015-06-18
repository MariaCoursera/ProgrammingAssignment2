setwd ("C:/Users/Owner/Desktop/Coursera/Data/assn_2")
#source ("cachematrix_testfcn.R")
options(digits=4)


# http://www.mathsisfun.com/algebra/matrix-introduction.html
# http://www.mathsisfun.com/algebra/matrix-determinant.html
# http://www.stat.columbia.edu/~martin/W4315/R1.pdf

  
  ## Initialize Test Data    
  c1 <- c(1,   .25)
  c2 <- c(.25, 1)
  TestMatrix1 <- cbind(c1,c2)
  
  c1 <- c(3,4,5)
  c2 <- c(8,6,9)
  c3 <- c(10,11,12)
  TestMatrix2 <- cbind(c1,c2,c3)
  
  c1 <- c(3,4,5,6)
  c2 <- c(7,8,9,10)
  c3 <- c(11,12,13,14)
  c4 <- c(15,16,17,18)
  TestMatrix3 <- rbind(c1,c2,c3,c4)

quick_check <- function(x=matrix()) {
  message (x$getmean())
  message(x$get_inverse())
  x$get()
}


## Test scenarios...

# New Cache(s)
env1<-makeCacheMatrix(TestMatrix1)
quick_check(env1)


# Try1
try1<-cacheSolve(env1)
quick_check(env1)


# Try2
try2 <- cacheSolve(env1)
quick_check(env1)


# Try3
try3 <- cacheSolve(env1)
quick_check(env1)


# Set matrix to something that won't create valid inverse
env1$set(TestMatrix3)
quick_check(env1)
solve(TestMatrix3)


# Try4
try4 <- cacheSolve(env1)
quick_check(env1)



## ---- Test 2 ----------------------
# New Cache
env1<-makeCacheMatrix(TestMatrix1)
quick_check(env1)
env2<-makeCacheMatrix(TestMatrix2)
quick_check(env2)
env3<-makeCacheMatrix(TestMatrix3)
quick_check(env3)


# Try1
try1_env1<-cacheSolve(env1)
quick_check(env1)


try1_env2<-cacheSolve(env2)
quick_check(env2)


try1_env3<-cacheSolve(env3)
quick_check(env3)


# Try2
try2_env1<-cacheSolve(env1)
quick_check(env1)


try2_env2<-cacheSolve(env2)
quick_check(env2)


try2_env3<-cacheSolve(env3)
quick_check(env3)


# Try3
try3 <- cacheSolve(env1)
quick_check(env1)


# Set matrix to something that won't create valid inverse
env1$set(TestMatrix3)
quick_check(env1)

env2$set(TestMatrix3)
quick_check(env1)


# Try4
try4_env1 <- cacheSolve(env1)
quick_check(env1)

try4_env2 <- cacheSolve(env2)
quick_check(env2)




 
