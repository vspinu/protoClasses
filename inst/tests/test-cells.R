context("Cells")

defaultContainer <- getClassDef("protoContext")@defaultContext
default <- defaultContainer[[".cells"]][["*"]]

x <- protoCell("x")
y <- x$new("y")
z <- y$new("zzz")

test_that("context initializes corectly", {
    expect_is(x, "protoCell")
    expect_is(y, "protoCell")
    expect_is(z, "protoCell")
    expect_error(protoCell("aaa", "bbb"))
})

test_that("Long type matches", {
    expect_equal(y$Type, "y.x.*")
    expect_equal(z$Type, "zzz.y.x.*")
    expect_equal(z$type, "zzz")
})

test_that("$ accessors return correct containers",{
    expect_that(z$methods, is_a("methodContainer"))
    expect_that(z$forms, is_a("formContainer"))
    expect_that(z$fields, is_a("fieldContainer"))
})

test_that("context inherit correctly", {
    expect_identical(as.environment(x$proto), as.environment(default))
    expect_identical(as.environment(y$proto), as.environment(x))
    expect_identical(as.environment(z$proto), as.environment(y))
    ## this one fails if code was sourced in global env
    expect_identical(parent.env(default), globalenv())
})

test_that("containers preserve inheritance", {
    expect_identical(parent.env(z$methods), as.environment(y$methods))
    expect_identical(parent.env(z$forms), as.environment(y$forms))
    expect_identical(parent.env(z$fields), as.environment(y$fields))
})

test_that("no spurious objects appear in containers", {
    expect_identical(.get_all_names(z$methods), character())
    expect_identical(.get_all_names(z$forms), character())
    expect_identical(.get_all_names(z$fields), character())
})


