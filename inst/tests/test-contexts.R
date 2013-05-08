context("protoContexts")

default <- getClassDef("protoContext")@defaultContext
X <- protoContext("X")
Y <- protoContext("Y", X)
Z <- Y$new("ZZZ")

test_that("context initializes corectly", {
    expect_is(X, "protoContext")
    expect_is(Y, "protoContext")
    expect_is(Z, "protoContext")
})

test_that("Long type matches", {
    expect_equal(Y$Type, "Y.X.@")
    expect_equal(Z$Type, "ZZZ.Y.X.@")
})

test_that("$ accessors return correct containers",{
    expect_that(Z$methods, is_a("methodContainer"))
    expect_that(Z$forms, is_a("formContainer"))
    expect_that(Z$fields, is_a("fieldContainer"))
    expect_that(Z$cells, is_a("cellContainer"))
})

test_that("context inherit correctly", {
    expect_identical(as.environment(X$proto), as.environment(default))
    expect_identical(as.environment(Y$proto), as.environment(X))
    expect_identical(as.environment(Z$proto), as.environment(Y))
})

test_that("containers preserve inheritance", {
    expect_identical(parent.env(Z$methods), as.environment(Y$methods))
    expect_identical(parent.env(Z$cells), as.environment(Y$cells))
    expect_identical(parent.env(Z$forms), as.environment(Y$forms))

    expect_identical(parent.env(default$methods), emptyenv())
    expect_identical(parent.env(default$fields), emptyenv())
    expect_identical(parent.env(default$forms), emptyenv())
    expect_identical(parent.env(default$cells), emptyenv())
})


test_that("no spurious objects appear in containers", {
    expect_identical(.get_all_names(Z$methods), c("debug", "inspect", "new", "undebug"))
    expect_identical(.get_all_names(Z$forms), character())
    expect_identical(.get_all_names(Z$fields),
                     c("cells", "fields", "forms", "methods", "proto", "rootCellParentEnv"))
    expect_identical(.get_all_names(Z$cells), "*")
})    


