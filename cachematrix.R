
## This function creates the cache of a previously calculated inverse matrix
makeCacheMatrix<-function(x=numeric()){
  s<-NULL
  set <- function(y) { #Set the value of the matrix
    x <<- y
    s <<- NULL
  }
  get<-function() x #Get's the value of the matrix
  setsolve<-function() s<<-solve(x) #Set the value of the inverse
  getsolve<-function() s #Get the value of the inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## This function search if the result of the inverse matrix has been obtained previously. If not, the inverse of the matrix is calculated
cacheSolve<- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) { #Check if the inverse has been calculated previously
    message("getting cached data")
    return(s) #Returns the result previously obtained
  }
  data <- x$get() 
  s <- solve(data, ...) #Calculates the result of the matrix if it has not been calculated preciously
  x$setsolve(s) 
  s
}