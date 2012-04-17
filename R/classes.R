if(existsMethod("initialize", "envProtoClass"))
    removeMethod("initialize", "envProtoClass")
if(existsMethod("initialize", "protoContext"))
    removeMethod("initialize", "protoContext")
if(existsMethod("initialize", "protoCell"))
    removeMethod("initialize", "protoCell")

###_ GENERIC METHODS:
setGeneric("installBinding",
           def = function(bindInfo, where, bindName, container = ".cells", ...) standardGeneric("installBinding"),
           useAsDefault = .installBinding_default)
setGeneric("initializeRoot", function(.Object, ...) standardGeneric("initializeRoot"))
setGeneric("clone",
           def = function(x, ...) standardGeneric("clone"),
           useAs = function(x, ...) x)


###_ FUNDAMENTAL OBJECTS:

###__ INFO:
## todo: change to protoCellInfo,protoFormInfo etc
## for other Infos default binding is ok? :todo

###___ cellInfo
setClass("cellInfo",
         representation(cellClass = "character",
                        names = "character"),
         prototype(cellClass = "protoCell"),
         contains = "list")

setMethod("installBinding", "cellInfo",
          .installBinding_cellInfo)

###___ formInfo
setClass("formInfo",
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
setMethod("$",
          signature(x = "envProtoClass"),
          definition = .dollarGet_envProtoClass
          )
setMethod("$<-",
          signature(x = "envProtoClass"),
          definition = .dollarSet_envProtoClass
          )

###__ protoContext
setClass("protoContext", contains = "envProtoClass")
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
setMethod("$",
          signature(x = "protoContext"),
          definition = .dollarGet_protoContext
          )
setMethod("$<-",
          signature(x = "protoContext"),
          definition = .dollarSet_protoContext
          )



###__ protoCell
setClass("protoCell", contains = "envProtoClass")
setMethod("initializeRoot", "protoCell",
          .initializeRoot_protoCell)

protoCell <- function(
               type = "--",
               prototype = NULL,
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
         representation = list(typeContainer = "character"),
         contains = "environment")
setMethod("initialize", signature(.Object = "protoContainer"),
          function(.Object, parentContainer = NULL, ...){
              ## tothink: .self is necessary?
              if(is.null(parentContainer)) parentContainer <- emptyenv()
              .Object <- callNextMethod()
              .Object@.xData <- new.env(TRUE, as.environment(parentContainer))
              .Object
          })
setMethod("clone", "protoContainer",
          .clone_protoContainer)

###__ cellContainer
setClass("cellContainer",
         prototype = prototype(typeContainer = ".cells"),
         contains = "protoContainer")
setMethod("show", signature(object = "protoContainer"),
          .show_Container)
setMethod("clone", "cellContainer",
          .clone_cellContainer)


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


.getPrototype <- function(cell)
    as.environment(cell)[[".prototype"]]
.setHomeContext <- function(cell, context)
    as.environment(cell)[[".homeContext"]] <- context

## Rgraphvis functionality
## setAs("cellContainer", "graphNEL", .as_cellContainer_graphNEL)
## setAs("protoContext", "graphNEL", .as_cellContainer_graphNEL)


## setMethod("plot", c("protoContext", "missing"),
##           def= plotCellGraph )
## setMethod("plot", c("protoContainer", "ANY"),
##           def = plotCellGraph)
