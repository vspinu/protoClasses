context("classes")

context_def <- getClassDef("protoContext")
cell_def <- getClassDef("protoCell")

test_that("class definition was modified", {
    expect_is(context_def@defaultContext, "protoContext")

    expect_equal(cell_def@contextClass, "protoContext")
})

test_that("parent environments are correct", {
    
    expect_equal(parent.env(context_def@defaultContext), getNamespace("protoClasses"))

})

test_that("initialization of derived protoContext classed works correctly", {
    setContextClass("testContext1", cellClass="protoCell")
    setContextClass("testContext2", cellClass="protoCell", contains = "testContext1")

    expect_equal(parent.env(getClassDef("testContext1")@defaultContext),
                 as.environment(getClassDef("protoContext")@defaultContext))

    expect_equal(parent.env(getClassDef("testContext2")@defaultContext),
                 as.environment(getClassDef("testContext1")@defaultContext))

    expect_is(getClassDef("testContext1")@defaultContext, "testContext1")

    expect_is(getClassDef("testContext2")@defaultContext, "testContext2")
})

