source("../R/funcs.R")
source("../R/classes.R")

X <- new("protoContext")
X
x <- new("protoCell", prot = "*", type = "x", expr = {appp <- 232323})
y <- new("protoCell", prototype = x, type = "y")
.infoCell(y)
.installCellInContext(y, X)
.infoCell(y)
z <- new("protoCell", type = "z", prototype = "y", homeContext = X)
.infoContext(X)
X
z

.installFields(list(fm = "matrix", fc = "character"), z)
z
z[[".fields"]][["fc"]]
z[[".fields"]][["fm"]]
z$fc <- 343
z$fc
z$fm <- 343
z$fm

z$sdfs <- 3
.installMethods(list(mf = function(x) print(.self) ), z)
.installMethods(list(mf = NULL), z)
z
z$mf()
z[[".methods"]][["mf"]]
z$mf()
.installForms(list(B = form(434*33)), z)
.installForms(list(C = 434*bb), z)
.installForms(list(B = NULL), z)
z

setContextClass("XXX")
X <- new("XXX", type = "X")
X
bbb <- X$new("bbb")

x <- new("protoCell", type = "x")
.installCellInContext(new("protoCell", prototype = "bbb", homeContext = X, type = "pp"), X)
sys.parents.objects(X[[".fields"]])
X
new("protoCell", prototype = "bbb", homeContext = X, type = "pp")

e <- new.env()
e[["a"]] <- 333
a <- 111

setClass("myfunc",
         representation =
         list(changeCallEnv = "logical",
              documentation = "character"),
         prototype = prototype(changeCallEnv = FALSE),
         contains = "function")

X <- new("protoContext")
X
x <- new("protoCell", proto = "*", type = "x")
y <- new("protoCell", prototype = x, type = "y")
.infoCell(y)
.installCellInContext(y, X)
X
x
X$`x.*` ## should be the same

x$forms(aa.bb.cc = form(43+4))
x
x$aa.bb
x$aa.bb.cc
x$forms(aa.bb.dd = form(D = 43+4,
          ss = a-b))
x$aa.bb
x$aa.bb.dd

y$forms(aa.bb.dd = form(D.pp = ssdsfdfs),
        aa.bb.pp = quote(34*aa),
        aa.bb.ee = expression(f-3))

ls(y)
y$aa.bb
y$aa.bb.dd
y$aa.bb.pp
y$aa.bb.ee
x

x$fields(a = TRUE, b = "blblb")
x[["a"]]
x[["b"]]
x$a <- 1
x$a
x$b <- "aaa"
x$b

x$fields(c("sss", "fff"))
x$sss <- "sfdsf"
x$sss
x[[".fields"]][["sss"]]

x$fields(list(dd = "matrix", pp = "charactersdf"))
x$dd <- "sdfs"
x$dd


x[[".homeContext"]]
.infoCell(y)
z <- new("protoCell", type = "z", prototype = "y", homeContext = X)
.infoContext(X)
X
z

X
clone(z[[".methods"]])
clone(z)


tf <- function(a, b, ...){
    list(missing(a), missing(b))
}


(tc <- cell(prototype = cell(type = "test"), type = "t2"))
(tc <- cell(prototype = "test", type = "t2"))
(tc <- cell(type = "t2",
            prototype =
            cell(type = "t1",
                 prototype =
                 cell(type = "t0"))))

(installBinding(tc, X))

.installCells(list(bbb =
                   cell(type = "cc",
                        prototype = cell(type = "cool")),
                   cell(type = "pc")), X)

