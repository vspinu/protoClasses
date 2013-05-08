context("Methods")

X <- protoContext()
Y <- protoContext(type="Y", X)

test_that("Initialization works",{
    X$initMethods(add = function(a, b) a + b)
    expect_true("add" %in% names(Y$methods))
    expect_equal(Y$add(1, 2), 3L)
})

test_that("Accessing withy '$methods'  works:",{
    expect_is(X$methods$add, "protoMethod")
    expect_error(X$methods$sfdsfdsfd)
    expect_equal(Y$methods$add(1,2), 3L) 
})

test_that("setMethod works:",{
    Y$setMethods(add = function(a, b) a/b)
    expect_equal(Y$add(1, 2), .5)
    Y$setMethods(.list = list(add = function(a, b) 10*a/b))
    expect_equal(Y$add(1, 2), 5)
    expect_equal(X$add(1, 2), 3)
    ## check methods interface assign proper environment to return functions:
    expect_equal(environment(Y$methods$add),  as.environment(Y))
})

test_that("Setting through $methods works:",{
    expect_error(Y$methods$non_existent <- function(a, b) a*b)
    Y$methods$add <- function(a, b) a*b
    expect_equal(Y$add(1, 2), 2)
})

test_that("Methods clear as expected:", {
    Y$initMethods(add = NULL)
    expect_equal(Y$add(1, 2), 3)
})

