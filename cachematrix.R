## code in the make makeCacheMatrix function sets four 
## behaviours that can be used to set and check cached matrices

makeCacheMatrix <- function( x=matrix() ) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function (solve) m<<-solve
      getinverse <- function() m
      list (set = set , get = get, setinverse = setinverse, getinverse = getinverse)
}


## code in this function uses the behaviours established
## in makeCacheMatrix to check and/or set inverse matrices

cacheSolve <- function(x, ...){
      m <- x$getinverse()
      ## code below checks for existing cached matrix that matches "m" in the applicable environment
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if no cached m, below code sets m to the inverse of x
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      (m)
}

        ## Return a matrix that is the inverse of 'x'
}
