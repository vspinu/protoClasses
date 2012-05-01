## working with contexts
(X <- protoContext())
(Y <- protoContext(type="Y", X))
X$type <- "343"
Y$type
Y$forms

## METHODS:
names(X$methods())
names(X$methods(list("expr", "initMethods")))
## arguments are unlisted and squashed
dput(names(X$methods(list("expr", list("initMethods")), c("expr"))))
names(X$methods("expr", "initMethods", "expr"))
names(X$methods(c("expr", "initMethods", "expr")))

X$initMethods(mymeth = function(a, b) a + b)
names(Y$methods())
Y$mymeth(1, 2)
Y$setMethods(mymeth = function(a, b) a/b)
Y$mymeth(1, 2)
Y$setMethods(.list = list(mymeth = function(a, b) 10*a/b))
Y$mymeth(1, 2)
Y$setMethods(mymeth1 = function(a, b) a/b) ## error

## X still calls the old method:
X$mymeth(1, 2)
## clear the method in Y
Y$initMethods(mymeth = NULL)
Y$mymeth(1, 2)


## FIELDS:
Y$type
X$initFields(aaa = "test string")
Y$aaa
Y$setFields(aaa = 435454)
Y$aaa <- 3434343
Y$aaa ## converted to character
X$aaa
Y$setFields(aaa = 34343)
Y$fields()
Y$initFields(fff = NULL)
Y$fields()
Y$fff <- "sdfdsf"

X$initFields(scale = 12,
             sigma = protoField(
               function(value){
                   if(missing(value)){
                       scale*sigma
                   }else{
                       assign("sigma", value/scale, .self)
                       invisible(value)
                   }}))

X$scale
X$sigma <- 1
X$sigma
X[["sigma"]]



Y$initFields(.list = list(ccc = 34L, ddd = 34), .classes=c(aaa = "ANY", bbb = "character"))
Y$bbb <- 232
Y$bbb
Y$aaa <- 232
Y$aaa
Y[[".fields"]][["aaa"]]
Y[[".fields"]][["bbb"]]

class(Y$ccc)
class(Y$ddd)
Y$ccc <- 100.1
Y$ccc

###_ FORMS
X$initForms(aa.bb = quote(print(type)))
with(Y, e(aa.bb))
with(X, e(aa.bb))
names(Y$aa.bb)
names(Y$aa)
Y$aa.bb
Y$initForms(aa.cc = quote(print(ccc)))
with(Y, e(aa))
with(X, e(aa))
str(Y$aa.bb)
Y$initForms(aa.dd = expression(print(ccc)))
str(Y$aa)

(X <- protoContext())
(Y <- protoContext(type="Y", X))

Y$initForms(ff = form(
              a1.cc.dd = form(print(type)),
              a2.b0 = form(z <- 34),
              a3 = form(print(z + 100))))

Y$ff
X$setForms(bb.a1 = quote(23)) ## error
print(Y$ff)
with(Y, e(ff))
Y$expr(e(ff))

Y$initForms(zz = form(
              a1.cc.dd = form(
                bb.cc = form(print(type))),
              a2.b0 = form(z <- 34),
              a3 = form(print(z + 100))))
print(Y$zz)

###_ CELLS
protoCell()
x <- protoCell(type = "x", prot = "*",  expr = {appp <- 232323})
x$expr(print(appp))
x$appp ## error

y <- protoCell(prototype = x, type = "ff")

.infoCell(y)

X$initCells(y)
## .installCellInContext(y, X)
.infoCell(y)

## CONTEXT CLASSES
setContextClass("XXX")
X <- new("XXX", type = "X")
X

X$initCells(bbb =
            cell(type = "cc",
                 prototype = cell(type = "cool")),
            cell(type = "pc"))

