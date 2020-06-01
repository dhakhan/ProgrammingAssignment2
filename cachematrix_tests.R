Sys.setlocale(category = "LC_MESSAGES", locale = "C")
source("tools.R")
source("cachematrix.R")

## unit tests for makeCacheMatrix function and its methods
attr(makeCacheMatrix, "name") <- "makeCacheMatrix"
TestFunction(
    TestedFunction = makeCacheMatrix,
    TestCases = list
    (
        list(desc   = "'matrix'",
             input  = alist(x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           }),
             class  = "list")
    )
)
TestFunction(
    TestedFunction = makeCacheMatrix,
    TestCases = list
    (
        list(desc   = "'no argument'",
             input  = alist(),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(),
                           x_1      = NULL),
             class  = "list"),
        list(desc   = "'matrix'",
             input  = alist(x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = NULL),
             class  = "list"),
        list(desc   = "'scalar'",
             input  = alist(x = 1),
             output = "argument should be a matrix",
             class  = "error message"),
        list(desc   = "'vector'",
             input  = alist(x = c(1, 2)),
             output = "argument should be a matrix",
             class  = "error message"),
        list(desc   = "'list'",
             input  = alist(x = list(1, "a")),
             output = "argument should be a matrix",
             class  = "error message"),
        list(desc   = "'data frame'",
             input  = alist(x = data.frame(a = 1:3, b = c("a", "b", "c"))),
             output = "argument should be a matrix",
             class  = "error message")
    ),
    TestEnvironment = TRUE
)

m <- makeCacheMatrix()
attr(m$get, "name")     <- "m$get"
attr(m$getinv, "name")  <- "m$getinv"
attr(m$set, "name")     <- "m$set"
attr(m$setinv, "name")  <- "m$setinv"

TestFunction(
    TestedFunction = m$set,
    TestCases = list
    (
        list(desc   = "'matrix argument - no output'",
             input  = alist(matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)),
             output = NULL,
             class  = NULL),
        list(desc   = "'non matrix argument'",
             input  = alist("a"),
             output = "argument should be a matrix",
             class  = "error message")
    )
)
TestFunction(
    TestedFunction = m$set,
    TestCases = list
    (
        list(desc   = "'matrix argument - environment",
             input  = alist(matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = NULL),
             class  = "list")
    ),
    TestEnvironment = TRUE
)
TestFunction(
    TestedFunction = m$get,
    TestCases = list
    (
        list(desc   = "'no arguments'",
             input  = alist(),
             output = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
             class  = "matrix"),
        list(desc   = "'argument not expected'",
             input  = alist(a),
             output = "unused argument (a)",
             class  = "error message")
    )
)
TestFunction(
    TestedFunction = m$get,
    TestCases = list
    (
        list(desc   = "'no arguments - environment'",
             input  = alist(),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = NULL),
             class  = "list")
    ),
    TestEnvironment = TRUE
)
TestFunction(
    TestedFunction = m$setinv,
    TestCases = list
    (
        list(desc   = "'matrix argument - no output'",
             input  = alist(matrix(data = c(4, 3, 2, 1), nrow = 2, ncol = 2)),
             output = NULL,
             class  = NULL),
        list(desc   = "'non matrix argument'",
             input  = alist("a"),
             output = "argument should be a matrix",
             class  = "error message")
    )
)
TestFunction(
    TestedFunction = m$setinv,
    TestCases = list
    (
        list(desc   = "'matrix argument - environment",
             input  = alist(matrix(data = c(4, 3, 2, 1), nrow = 2, ncol = 2)),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = matrix(data = c(4, 3, 2, 1), nrow = 2, ncol = 2)),
             class  = "list")
    ),
    TestEnvironment = TRUE
)
TestFunction(
    TestedFunction = m$getinv,
    TestCases = list
    (
        list(desc   = "'no arguments'",
             input  = alist(),
             output = matrix(data = c(4, 3, 2, 1), nrow = 2, ncol = 2),
             class  = "matrix"),
        list(desc   = "'argument not expected'",
             input  = alist(a),
             output = "unused argument (a)",
             class  = "error message")
    )
)
TestFunction(
    TestedFunction = m$getinv,
    TestCases = list
    (
        list(desc   = "'no arguments - environment'",
             input  = alist(),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = matrix(data = c(4, 3, 2, 1), nrow = 2, ncol = 2)),
             class  = "list")
    ),
    TestEnvironment = TRUE
)

## unit tests for cacheSolve function
attr(cacheSolve, "name") <- "cacheSolve"
m <- makeCacheMatrix(matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2))
TestFunction(
    TestedFunction = cacheSolve,
    TestCases = list
    (
        list(desc   = "'positive scenario'",
             input  = alist(x = m),
             output = list(get      = function() x,
                           getinv   = function() x_1,
                           set      = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x <<- y
                               x_1 <<- NULL
                           },
                           setinv   = function(y)
                           {
                               if  (!is.matrix(y))
                                   stop("argument should be a matrix")
                               x_1 <<- y
                               NULL
                           },
                           x        = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2),
                           x_1      = matrix(data = c(-2, 1, 1.5, -0.5), nrow = 2, ncol = 2)),
             class  = "list"),
        list(desc   = "'invalid environment'",
             input  = alist(x = list(get      = function() x,
                                     getinv   = function() x_1,
                                     set      = function(y)
                                     {
                                         if  (!is.matrix(y))
                                             stop("argument should be a matrix")
                                         x <<- y
                                         x_1 <<- NULL
                                     },
                                     setinv   = function(y)
                                     {
                                         if  (!is.matrix(y))
                                             stop("argument should be a matrix")
                                         x_1 <<- y
                                         NULL
                                     })),
             output = "object 'x_1' not found",
             class  = "error message"),
        list(desc   = "'scalar'",
             input  = alist(x = 1),
             output = "argument should be a list generated by makeCacheMatrix function",
             class  = "error message"),
        list(desc   = "'vector'",
             input  = alist(x = c(1, 2)),
             output = "argument should be a list generated by makeCacheMatrix function",
             class  = "error message"),
        list(desc   = "'list'",
             input  = alist(x = list(1, "a")),
             output = "argument should be a list generated by makeCacheMatrix function",
             class  = "error message"),
        list(desc   = "'data frame'",
             input  = alist(x = data.frame(a = 1:3, b = c("a", "b", "c"))),
             output = "argument should be a list generated by makeCacheMatrix function",
             class  = "error message")
    ),
    EnvironmentToTest = environment(m$get)
)

Sys.setlocale(category = "LC_MESSAGES", locale = "")
