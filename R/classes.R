if(existsMethod("initialize", "envProtoClass"))
    removeMethod("initialize", "envProtoClass")
if(existsMethod("initialize", "protoContext"))
    removeMethod("initialize", "protoContext")
if(existsMethod("initialize", "protoCell"))
    removeMethod("initialize", "protoCell")

###_ GENERIC METHODS:
setGeneric("installBinding",
           def = function(bindInfo, container, bindName, ...) standardGeneric("installBinding"),
           useAsDefault = .installBinding_default)
setGeneric("initializeRoot", function(.Object, ...) standardGeneric("initializeRoot"))
setGeneric("clone",
           def = function(x, ...) standardGeneric("clone"),
           useAs = function(x, ...) x)


###_ FUNDAMENTAL OBJECTS:

###__ INFO:
###___ protoCellInfo
setClass("protoCellInfo",
         representation(cellClass = "character",
                        names = "character"),
         prototype(cellClass = "protoCell"),
         contains = "namedList")

setMethod("installBinding", "protoCellInfo",
          .installBinding_protoCellInfo)

###___ protoFormInfo
setClass("protoFormInfo",
         representation(formClass = "character"),
         prototype(formClass = "protoForm")) # add host protoObject?


###__ CORE:
setClass("protoFunction",
         representation(changeCallEnv = "logical",
                        doc = "character"),
         prototype(changeCallEnv = FALSE),
         contains = "function")
###___ protoMethod
setClass("protoMethod",
         representation(bindName = "character",
                        container = "character"),
         ## todo: change to typeContainer
         prototype(container = ".methods"),
         contains = "protoFunction")

###___ protoField
setClass("protoField",
         representation(bindName = "character",
                        installedObjectNames = "character",
                        className = "character",
                        container = "character"),
         prototype(container = ".fields",
                   bindName = "dummy_field"),
         contains = "protoFunction")
setMethod("initialize", signature(.Object = "protoField"),
          .initialize_Field)
protoField <- function(func = function(value) NULL ,  doc = "", ...)
    new("protoField", func, doc = doc, ...)

setMethod("installBinding", "protoField",
          .installBinding_protoField)

###___ protoForm
##' Class to represent protoForms.
##'
##' protoForms are expressions which can contain protoForms as
##' subexpressions. Slots are:
##' @export
## @slot names character vector providing names for each subexpression or
## subform.
## @slot doc Self contained documentation.
setClass("protoForm",
         representation(names = "character",
                        doc = "character"),
         ## level = "numeric",
         ## cell = "environment"),
         contains = "expression")

setMethod("installBinding", "protoForm",
          .installBinding_protoForm)
setAs("ANY", "protoForm",
      as.form)  ## todo: qualify as.pbm.sexp
setAs("expression", "protoForm",
      ## why  .makeNames here? new have to create them todo:
      function(from){
          names <- .makeNames(allNames(from))
          new("protoForm", from, names = names)
      })
setAs("call", "protoForm",
      function(from){
          new("protoForm", as.expression(from))
      })
setMethod("initialize", "protoForm",
          function(.Object, ...){
              .Object <- callNextMethod()
              ## forms always have names!!
              names(.Object) <- .makeNames(allNames(.Object))
              .Object
          })

##' Subset protoForm objects.
##'
##' @param x protoForm object
##' Return a protoForm object.
setMethod("[",
          signature(x = "protoForm"),
          function (x, i, j, ..., drop = TRUE){
              ## why [ strips class in the first place todo:
              as(callNextMethod(), "protoForm")
          })


###___ protoFormWithEnv
setClass("protoFormWithEnv",
         representation(environment = "environment"),
         contains = "protoForm") ## used for printing, $ methods returns this object with environment slot set to .self
setMethod("show", signature(object = "protoFormWithEnv"),
          .show_ProtoFormWithEnv)
setMethod("print", signature(x = "protoFormWithEnv"),
          .print_ProtoFormWithEnv)

###___ protoFormWithBrowser
setClass("protoFormWithBrowser",
         representation(original = "protoForm",
                        hostEnv = "environment",
                        isInHost = "logical"),
         contains = "protoForm")


###_ PROTO:
###__ envProtoClass

##' Parent S4 class of all proto objects.
##'
##' \link{protoContext-class} and \link{protoCell-class}
##' @export
setClass("envProtoClass", contains = "environment")
setMethod("initializeRoot", "envProtoClass",
          .initializeRoot_envProtoClass)
setAs("environment", "envProtoClass", function(from){
    from <- callNextMethod()
    print("COOOl")
    from
})
setMethod("show", signature(object = "envProtoClass"),
          .show_EnvProtoObject)
setMethod("clone", "envProtoClass",
          .clone_envProtoClass)

##' Dollar accessors of envProtoClasses
##'
##' "$" syntax is a multifunctional accessor for \code{envProtoClasses}
##' todo:....
##' @rdname dollar
##' @param x an object extending envProtoClass
##' 
setMethod("$",
          signature(x = "envProtoClass"),
          definition = .dollarGet_envProtoClass
          )

##'
##' @rdname dollar
setMethod("$<-",
          signature(x = "envProtoClass"),
          definition = .dollarSet_envProtoClass
          )

###__ protoContext
##' Class to represent proto contexts
##' @export
setClass("protoContext", contains = "envProtoClass")

##' Constructor for protoContext objects
##'
##' @inheritParams protoCell
##' @param cells ...
##' @param initCells ...
##' @param rootParentEnv ...
##' @return an object of class protoContext
##' @author Vitalie Spinu
protoContext <- function(
                  type = "--",
                  prototype = NULL,
                  fields = list(),
                  methods = list(),
                  forms = list(),
                  cells = list(),
                  initFields = list(),
                  initMethods = list(),
                  initForms = list(),
                  initCells = list(),
                  expr = expression(),
                  rootParentEnv = NULL, ## todo:  make field
                  ...){
    new("protoContext", type = type, prototype = prototype,
        initCells = initCells, cells = cells,
        initMethods = initMethods, methods = methods,
        initFields = initFields, fields = fields,
        initForms = initForms, forms = forms,
        expr = expr, ...)}


setMethod("initializeRoot", "protoContext",
          .initializeRoot_protoContext)
setMethod("show", signature(object = "protoContext"),
          .show_Context)
setMethod("clone", "protoContext",
          .clone_protoContext)

##' @rdname dollar
setMethod("$",
          signature(x = "protoContext"),
          definition = .dollarGet_protoContext
          )

##' @rdname dollar
setMethod("$<-",
          signature(x = "protoContext"),
          definition = .dollarSet_protoContext
          )


###__ protoCell
##' Class to represent cells.
##'
##' See \link{protoCell} constructor for more details.
##' @export
setClass("protoCell", contains = "envProtoClass")
setMethod("initializeRoot", "protoCell",
          .initializeRoot_protoCell)


##' Default constructor of proto cells.
##'
##' Proto Cells recide inside protoContext in special container
##' \code{.cells}. Proto Cells also derive from \link{envProtoClass} and exhibit
##' normal prototype behavior, in the sence that each cell inherit it's methods,
##' fields and forms form parent cells.
##'
##' @param type type
##' @param prototype parent cell
##' @param fields ...
##' @param methods ...
##' @param forms ...
##' @param initFields ...
##' @param initMethods ...
##' @param initForms ...
##' @param expr ...
##' @param rootParentEnv ...
##' @param ... ...
##' @return object of class \link{protoCell-class}.
##' @author Vitalie Spinu
##' @export
protoCell <- function(
               type = "--",
               prototype = "*",
               fields = list(),
               methods = list(),
               forms = list(),
               initFields = list(),
               initMethods = list(),
               initForms = list(),
               expr = expression(),
               rootParentEnv = NULL, ## todo:  make field
               ...)
    new("protoCell", type = type, prototype = prototype,
        initMethods = initMethods, methods = methods,
        initFields = initFields, fields = fields,
        initForms = initForms, forms = forms,
        expr = expr, ...)

setMethod("installBinding", "protoCell",
          .installBinding_protoCell)

###__ protoContainer
setClass("protoContainer",
         representation = list(typeContainer = "character", host = "envProtoClass"),
         contains = "environment")
setMethod("initialize", signature(.Object = "protoContainer"),
          function(.Object, ...){
              .Object <- callNextMethod()
              parentContainer <- 
                  if(is.null(prot <- .getPrototype(.Object@host))){
                      emptyenv()
                  }else{
                      prot[[.Object@typeContainer]]
                  }
              env <- new.env(TRUE, as.environment(parentContainer))
              .Object@.xData <- env
              .Object
          })
setMethod("$",
          signature(x = "protoContainer"),
          definition = function(x, name) get(name, envir = x))

setMethod("clone", "protoContainer",
          .clone_protoContainer)
setMethod("show", signature(object = "protoContainer"),
          .show_Container)

setGeneric("specialNames", def = function(protoObject) standardGeneric("specialNames"),
           useAsDefault = function(protoObject) character())

###__ formContainer
setClass("formContainer",
         prototype = prototype(typeContainer = ".forms"),
         contains = "protoContainer")
setMethod("$", signature(x = "formContainer"),
          .dollarGet_formContainer)
setMethod("$<-", signature(x = "formContainer"),
          function(x, name) .dollarSet_formContainer(x, name, value))

###__ methodContainer
setClass("methodContainer",
         prototype = prototype(typeContainer = ".methods"),
         contains = "protoContainer")
setMethod("$",
          signature(x = "methodContainer"),
          .dollarGet_methodContainer )
setMethod("$<-", signature(x = "methodContainer"),
          function(x, name, value) .dollarSet_methodContainer(x, name, value))
setMethod("specialNames", "methodContainer",
          function(protoObject) c("cells", "expr", "fields", "forms", "initCells", "initFields", 
                                  "initForms", "initMethods", "methods", "setFields", "setForms", 
                                  "setMethods"))

###__ fieldContainer
setClass("fieldContainer",
         prototype = prototype(typeContainer = ".fields"),
         contains = "protoContainer")
setMethod("$", signature(x = "fieldContainer"),
          .dollarGet_fieldContainer)
setMethod("$<-", signature(x = "fieldContainer"),
          function(x, name, value) .dollarSet_fieldContainer(x, name, value))
setMethod("specialNames", "fieldContainer",
          function(protoObject) c("f", "h", "m", "type", "Type"))
          

###__ cellContainer
setClass("cellContainer",
         prototype = prototype(typeContainer = ".cells"),
         contains = "protoContainer")
setMethod("clone", "cellContainer",
          .clone_cellContainer)
setMethod("$", signature(x = "cellContainer"),
          .dollarGet_cellContainer)

###_ CLASS REPRESENTATIONS:
## Extension of classRepresentation to store two new slots: defaultContext and
## classDef. each new protoContext class generates new classDef with it's
## default context.
assignClassDef("envProtoClass", .modifyAs(getClassDef("envProtoClass")))

###__ contextClassRepresentation
setClass("contextClassRepresentation",
         representation(defaultContext = "protoContext",
                        cellClass = "character"),
         prototype = list(cellClass = "protoCell"),
         contains = "classRepresentation")
setMethod("show", "contextClassRepresentation",
          function(object) .show_ContextClassDef(object))
assignClassDef("protoContext",
               .modifyAs(new("contextClassRepresentation", getClassDef("protoContext") ,
                             defaultContext = newRoot("protoContext", type = "@", isDefaultContext = TRUE))
                         ))

###__ cellClassRepresentation
setClass("cellClassRepresentation",
         representation(contextClass = "character"),
         ## really need it?
         prototype = list(contextClass = "protoContext"),
         contains = "classRepresentation")
assignClassDef("protoCell",
               .modifyAs(new("cellClassRepresentation", getClassDef("protoCell"))))

## Install the super-root cell in the super - root default context
getClassDef("protoContext")@defaultContext$initCells(`*` = newRoot("protoCell"))
invisible(NULL)

###_ INITIALIZE:
## (really should be at the end)
setMethod("initialize", signature(.Object = "envProtoClass"),
          .initialize_envProtoClass)
setMethod("initialize", signature(.Object = "protoContext"),
          .initialize_protoContext)
setMethod("initialize", signature(.Object = "protoCell"),
          .initialize_protoCell)

###_ MISC:
setGeneric("setPrototype",
           def = function(protoObj, prototype) standardGeneric("setPrototype"),
           useAsDefault =
           function(protoObj, prototype){
               assign(".prototype", prototype, envir = protoObj)
               parent.env(protoObj) <- prototype
               .fields <- get(".fields", envir = protoObj)
               parent.env(.fields) <- get(".fields", envir = prototype)
               .forms <- get(".forms", envir = protoObj)
               parent.env(.forms) <- get(".forms", envir = prototype)
               .methods <- get(".methods", envir = protoObj)
               parent.env(.methods) <- get(".methods", envir = prototype)
           })

setMethod("setPrototype", "protoContext",
          function(protoObj, prototype){
              callNextMethod()
              parent.env(get(".cells", envir = protoObj)) <- parent.env(get(".cells", envir = prototype))
          })


## Rgraphvis functionality
## setAs("cellContainer", "graphNEL", .as_cellContainer_graphNEL)
## setAs("protoContext", "graphNEL", .as_cellContainer_graphNEL)


## setMethod("plot", c("protoContext", "missing"),
##           def= plotCellGraph )
## setMethod("plot", c("protoContainer", "ANY"),
##           def = plotCellGraph)


