context("Methods\t")

X <- protoContext()
Y <- protoContext(type="Y", X)

test_that("Initialization works",{
    X$initMethods(mymeth = function(a, b) a + b)
    expect_equal(names(Y$methods()), "mymeth")
    expect_equal(Y$mymeth(1, 2), 3L)
})

test_that("Accessing withy 'methods'  works:",{
    expect_is(X$methods("mymeth"), "list")
    expect_is(X$methods("mymeth")[[1L]], "protoMethod")
    expect_error(X$methods("sfdsfdsfd"))
})

test_that("Accessing with '$m$' works:",{
    expect_is(Y$m$mymeth, "protoMethod")
    expect_equal(Y$m$mymeth(1,2), 3L) 
    expect_error(X$m$sdfdsfdf)
})

test_that("setMethod works:",{
    Y$setMethods(mymeth = function(a, b) a/b)
    expect_equal(Y$mymeth(1, 2), .5)
    Y$setMethods(.list = list(mymeth = function(a, b) 10*a/b))
    expect_equal(Y$mymeth(1, 2), 5)
    ## check methods interface assign proper environment to return functions:
    expect_equal(Y$methods("mymeth")[[1]](1,2), 5,
                 info = "check methods interface assign proper environment to return functions")
    expect_error(Y$setMethods(mymeth1 = function(a, b) a/b))
    ## X still calls the old method:
    expect_equal(X$mymeth(1, 2), 3L)
})

test_that("Setting through $m$ works:",{
    Y$m$mymeth <- function(a, b) a*b
    expect_equal(Y$mymeth(1, 2), 2)
})

test_that("Methods clear as expected:", {
    Y$initMethods(mymeth = NULL)
    expect_equal(Y$mymeth(1, 2), 3)
})

