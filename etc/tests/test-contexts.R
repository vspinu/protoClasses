context("Contexts")

X <- protoContext("X")
Y <- protoContext("Y", X)

test_that("Long type matches", expect_equal(.getType(Y), "Y.X.@"))

test_that("$ and $<- do a proper job",{
    expect_that(X$methods, is_a("protoMethod"))
    expect_true(all(sapply(X$methods(), is, "protoMethod")))
    ## special names are not reported
    expect_identical(names(X$methods()), character())
    ## special names
    expect_identical(setdiff(specialNames(X[[".methods"]]),
                             c("cells", "expr", "fields", "forms", "initCells", "initFields", 
                               "initForms", "initMethods", "methods", "setFields", "setForms", 
                               "setMethods")), character())
    expect_identical(names(X$methods("expr", "initMethods")), c("expr", "initMethods"))
    out <- c("expr", "initMethods", "expr")
    expect_identical(names(X$methods(list("expr", list("initMethods")), c("expr"))), out)
    expect_identical(names(X$methods("expr", "initMethods", "expr")), out)
    expect_identical(names(X$methods(c("expr", "initMethods", "expr"))), out)
})    
    
