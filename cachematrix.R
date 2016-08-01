#makeCacheMatrix() and cacheSolve() are two functions here to furnish the programming assignment. 
#The former defines the child functions to either set or get the matrix m value and set or get its inverse as well.
#The later function fetches the inverse of input matrix if available already and calculates the inverse of input matrix if not available. It also puts back the calculated inverse into memory for later retrieval.


# makeCacheMatrix function is created in similar to the makeVector function provided in the assignment question. 
#The function takes a matrix m as input. #It intializes i as a zero matrix to capture the inverse in the beginning. It can just be declared i<- NULL as well (both works). 
#Within makeCacheMatrix, the child functions: set(),get(), setinv() and getinv() are defined. 
#set() is used to assign the input argument y to matrix m. 
#get() is to get the matrix m. 
#setinv() is to assign the input inverse matrix to i
#getinv() is to get the inverse matrix i of m 

makeCacheMatrix <- function(m = matrix()) {
        i <- matrix(0L, dim(m)[1],dim(m)[2])
        set <- function(y) {
                m <<- y
                i <<- matrix(0L, dim(y)[1],dim(y)[2])
        }
        get <- function() m
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function takes the matrix m as input argument. It uses the getinv function to gets its inverse in i. 
#The expresssion !all(i==0) is verifies if the inverse in i does not have all of its elements as zero.
#If true, it returns the inverse value. Otherwise, it assigns the input matrix m to variable data, 
#calculates its inverse using solve() and assigns it to i in subsequent lines of code. 
#setinv() is next used to put back the calculated inverse value to the parent variables in makeCacheMatrix function.

cacheSolve <- function(m, ...) {
        i <- m$getinv()
        if(!all(i == 0)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinv(i)
        i
}

##Testing
#>m = matrix(c(2, 4, 3, 1, 5, 7, 3, 6, 9), nrow=3, ncol=3)
#> m
#     [,1] [,2] [,3]
#[1,]    2    1    3
#[2,]    4    5    6
#[3,]    3    7    9
#> c = makeCacheMatrix(m)
#> cacheSolve (c)
#          [,1]       [,2]          [,3]
#[1,]  0.1111111  0.4444444 -3.333333e-01
#[2,] -0.6666667  0.3333333  1.708035e-17
#[3,]  0.4814815 -0.4074074  2.222222e-01

## please note the getting cached data message getting displayed when cacheSolve function is called the second time.
#> cacheSolve (c) 
#getting cached data
#           [,1]       [,2]          [,3]
#[1,]  0.1111111  0.4444444 -3.333333e-01
#[2,] -0.6666667  0.3333333  1.708035e-17
#[3,]  0.4814815 -0.4074074  2.222222e-01
#> i <- cacheSolve(c)

## Multiplying a matrix with its inverse will always result in the below result 
#> round(m %*% i)
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

