#TEST
######################################################
#> source('C:/Users/PcCom/Desktop/Exercicis R/funcions.R')
#> matriu<- matrix(1:4,2,2)
#> my_matrix<-makeCacheMatrix(matriu)
#> my_matrix$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> my_matrix$getInverse()
#NULL
#> cacheSolve(my_matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(my_matrix)
#Getting data from cache...
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
##################################################

makeCacheMatrix<-function(x=matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setInverse <- function(invers) inv <<- solve(invers)
  
  getInverse <- function() inv
  
  list(set=set,get=get,setInverse=setInverse,
       getInverse=getInverse )
}


cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting data from cache...")
    return(inv)
  }
  
  matr<- x$get()
  inv <- x$setInverse(matr)
  inv
}
