context("Fields\t")


X <- protoContext()
Y <- protoContext(type="Y", X)

test_that("Basic fields work",{
    expect_equal(Y$type, "Y")
    expect_equal(Y$Type, "Y.--.@")
    expect_warning(X$type <- "aa.bb")
    expect_equal(X$type, "aa_bb")
    expect_error(X$Type <- "sdfds.dffs")
    expect_is(X$m, "methodContainer")
    expect_is(X$f, "fieldContainer")
    expect_identical(Y$f@host, Y)
})
          
test_that("Initialization works",{
    X$initFields(aaa = "test")
    X$initFields(bbb = 232L)
    expect_equal(Y$aaa, "test")
    expect_identical(Y$bbb, 232L)
})

test_that("Accessing with 'fields' works",{
    expect_equal(names(Y$fields()), c("aaa", "bbb"))
    expect_is(X$fields("aaa"), "list")
    expect_is(X$fields("aaa","bbb")[[1L]], "character")
    expect_error(X$fields("sfdsfdsfd"))
    ## recursive lists work for accessor:
    expect_equal(length(X$fields(list("aaa"),"bbb", list("aaa", list("bbb", "bbb")))), 5L)
})

test_that("Accessing with '$f$' works:",{
    expect_is(Y$f$aaa, "character")
    expect_equal(Y$f$aaa, "test")
    expect_identical(Y$f$bbb, 232L) 
    expect_error(X$m$sdfdsfdf)
})

test_that("setField works:",{
    Y$setFields(bbb = 43.2)
    expect_identical(Y$bbb, 43L)
    Y$aaa <- 3333
    expect_identical(Y$aaa, "3333")
    expect_identical(X$aaa, "test")
    Y$setFields(aaa = 555, bbb = 0)
    ## recursive lists don't work for setters:'
    ## use .list to assign list of defaults:
    expect_error(Y$setFields(list(aaa = 555, bbb = 0)))
    Y$setFields(.list = list(aaa = 555, bbb = 0))
    expect_identical(Y$fields(list("aaa", "bbb")),
                     list(aaa = "555", bbb = 0L))
    expect_error(Y$setFields(ccc = function(a, b) a/b))
    X$initFields(ccc = function(a) a^5)
    expect_is(Y$ccc, "function")
    expect_equal(Y$ccc(2), 32)
})

test_that("Setting with $f$ works:",{
    Y$f$aaa <- 44
    expect_equal(Y$f$aaa, "44")
    expect_equal(Y$aaa, "44")
})

test_that("Fields clear as expected:", {
    ## not initialized in Y, so cannot remove
    ## todo: give more meaningful error message!!
    expect_warning(Y$initFields(aaa = NULL))
    X$initFields(aaa = NULL)
    expect_equal(length(X$fields()), 2L) ## bbb, ccc
    expect_error(X$f$aaa)
    expect_error(Y$aaa)   
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
    Y$initFields(.list = list(ccc = 34L, ddd = 34), .classes=c(aaa = "ANY", bbb = "character"))
    Y$bbb <- 232
    expect_equal(Y$bbb, "232")
    Y$aaa <- 232
    expect_is(Y$aaa, "numeric")
    Y$aaa <- "text"
    expect_is(Y$aaa, "character")
})
