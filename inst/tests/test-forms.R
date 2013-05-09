context("Forms")

x <- protoCell("x")

f <- form(
  first =
  form({a <- 23
        a + 3},
       form(pp <- 34)),
  second = {
      b <- 100
      a + b
  })

test_that("recursive form initialization works", {
    expect_is(f, "protoForm")
    expect_equal(names(f), c("first", "second"))
    expect_is(f[["first"]], "protoForm")
    expect_equal(names(f[["first"]]), c("A", "B"))
    expect_is(f[["first"]][["B"]], "protoForm")
    expect_is(f[["second"]], "{")
})

x$initForms(A1 = form(
              B1 = form(
                C = 1 + 2),
              B2 = form(
                C = 2 * 3)),
            A2 = form(
              B1 = form(
                C = 10 + 20),
              B2 = form(
                C = 20 * 30)))


test_that("forms are installed correctly", {
    expect_equal(names(x$forms), c("A1", "A1.B1", "A1.B2", "A2", "A2.B1", "A2.B2"))
    expect_is(x$A1, "protoFormWithEnv")
    expect_equivalent(c(x$A1), expression(B1 = e(A1.B1), B2 = e(A1.B2)))
    expect_equivalent(c(x$A1.B1), expression(C = 1 + 2))
    expect_error(x$A1.B1.C)
})

test_that("forms are installed correctly", {
    expect_equal(names(x$forms), c("A1", "A1.B1", "A1.B2", "A2", "A2.B1", "A2.B2"))
    expect_is(x$A1, "protoFormWithEnv")
    expect_is(x$A1.B1, "protoFormWithEnv")
    expect_identical(names(x$A1), c("B1", "B2"))
    expect_equivalent(c(x$A1), expression(B1 = e(A1.B1), B2 = e(A1.B2)))
    expect_equivalent(c(x$A1.B1), expression(C = 1 + 2))
    expect_error(x$A1.B1.C)
    expect_true(isECall(x$A1[[1]]))
})
