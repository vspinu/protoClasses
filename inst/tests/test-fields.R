context("Fields")

X <- protoContext()
Y <- protoContext(type="Y", X)
X$initFields(string = "test")
X$initFields(integer = 232L)

test_that("Basic fields work",{
    expect_equal(Y$type, "Y")
    expect_equal(Y$Type, "Y.--.@")
    expect_warning(X$type <- "aa.bb")
    expect_equal(X$type, "aa_bb")
    expect_error(X$Type <- "sdfds.dffs")
    expect_is(X$methods, "methodContainer")
    expect_is(X$fields, "fieldContainer")
    expect_identical(Y$fields@host, Y)
    expect_identical(Y$cells@host, Y)
})
          
test_that("Field initialization works",{
    expect_equal(Y$string, "test")
    expect_identical(Y$integer, 232L)
})

test_that("Accessing with '$fields' works",{
    expect_true(all( c("string", "integer") %in% names(Y$fields)))
    expect_identical(X$fields$string, "test")
    expect_identical(Y$fields$string, "test")
    expect_identical(Y$fields$integer, 232L)
    expect_error(X$fields$sfdsfdsfd)
    ## recursive lists work for accessor:
    expect_equal(length(names(X$fields)), 8L)
})

test_that("setField works:",{
    Y$setFields(integer = 43.2)
    expect_identical(Y$integer, 43L)
    Y$string <- 3333
    expect_identical(Y$string, "3333")
    expect_identical(X$string, "test")
    Y$setFields(string = 555, integer = 0)
    ## recursive lists don't work for setters:'
    ## use .list to assign list of defaults:
    expect_error(Y$setFields(list(string = 555, integer = 0)))
    Y$setFields(.list = list(string = 555, integer = 0))
    expect_identical(Y$fields$string, "555")
    expect_identical(Y$fields$integer, 0L)
    expect_error(Y$setFields(ccc = "aaa"))
    X$initFields(ccc = function(a) a^5)
    expect_is(Y$ccc, "function")
    expect_equal(Y$ccc(2), 32)
})

test_that("Setting with $fields works:",{
    Y$fields$string <- 44
    expect_equal(Y$fields$string, "44")
    expect_equal(Y$string, "44")
})

test_that("Fields clear as expected:", {
    ## not initialized in Y, so cannot remove
    ## todo: give more meaningful error message!!
    X$setFields(string = NULL)
    X$initFields(string = NULL)
    expect_warning(Y$initFields(string = NULL))
    X$initFields(string = NULL)
    expect_error(X$fields$string)
    expect_error(Y$string)   
})

test_that("Complex fields work:", {
    X$initFields(scale = 12,
                 sigma = protoField(
                   function(value){
                       if(missing(value)){
                           scale*sigma
                       }else{
                           assign("sigma", value/scale, .self)
                           invisible(value)
                       }}))
    X$sigma <- 1
    expect_equal(X$sigma, 1)
    expect_equal(X[["sigma"]], 1/12)
    X$scale <- 24
    expect_equal(X$sigma, 2)
})

test_that("Virtual (ANY) classes behave as expected",{
    Y$initFields(.list = list(ccc = 34L, ddd = 34),
                 .classes=c(any2 = "ANY", string = "character"))
    expect_identical(Y$string, character())
    expect_error(Y$any) # not initialized for ANY
    Y$string <- 232
    expect_equal(Y$string, "232")
    Y$any <- 232
    expect_is(Y$any, "numeric")
    Y$string <- "text"
    expect_identical(Y$string, "text")
})
