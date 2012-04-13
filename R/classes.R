###_ FUNDAMENTAL OBJECTS
###__ CLASESS
setClass("cellInfo",
         representation(cellClass = "character",
                        names = "character"),
         prototype(cellClass = "protoCell"),
         contains = "list")
setClass("formInfo",
         representation(formClass = "character"),
         prototype(formClass = "protoForm")) # add host protoObject?
setClass("protoFunction",
         representation(changeCallEnv = "logical",
                        doc = "character"),
         prototype(changeCallEnv = FALSE),
         contains = "function")
setClass("protoMethod",
         representation(bindName = "character",
                        container = "character"),
         ## todo: change to typeContainer
         prototype(container = ".methods"),
         contains = "protoFunction")
setClass("protoField",
         representation(bindName = "character",
                        installedObjectNames = "character",
                        className = "character",
                        container = "character"),
         prototype(container = ".fields"),
         contains = "protoFunction")
setClass("protoForm",
         representation(names = "character",
                        doc = "character"),
         ## level = "numeric",
         ## cell = "environment"),
         contains = "expression")
setClass("protoFormWithEnv",
         representation(environment = "environment"),
         contains = "protoForm") ## used for printing, $ methods returns this object with environment slot set to .self
setClass("protoFormWithBrowser",
         representation(original = "protoForm",
                        hostEnv = "environment",
                        isInHost = "logical"),
         contains = "protoForm")

###_ METHODS
## elements of a form are guarantied to be named
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
setMethod("initialize", signature(.Object = "protoField"),
          .initialize_Field)
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

###_ PROTO - OBJECTS
###__ CLASSES
removeMethod("initialize", "envProtoClass")
removeMethod("initialize", "protoCell")
removeMethod("initialize", "protoContext")
removeMethod("initialize", "protoContainer")
setClass("envProtoClass", contains = "environment")
## Containers (protoCell initialization depends on this)
setClass("protoContext",
         contains = "envProtoClass")
setClass("protoCell",
         ## representation =
         ## list(contextClass = "character"),
         ## prototype = list(contextClass = "protoContext"),
         contains = "envProtoClass")
setClass("protoContainer",
         representation = list(typeContainer = "character"),
         contains = "environment")
setClass("cellContainer",
         prototype = prototype(typeContainer = ".cells"),
         contains = "protoContainer")

#### METHODS
## installBinding
setGeneric("installBinding",
           def = function(bindInfo, where, bindName, container = ".cells", ...) standardGeneric("installBinding"),
           useAsDefault = .installBinding_default)
## todo: change to protoCellInfo,protoFormInfo etc
## for other Infos default binding is ok? :todo
setMethod("installBinding", "cellInfo",
          .installBinding_cellInfo)
setMethod("installBinding", "protoCell",
          .installBinding_protoCell)
setMethod("installBinding", "protoForm",
          .installBinding_protoForm)
setMethod("installBinding", "protoField",
          .installBinding_protoField)
## initialize
setMethod("initialize", signature(.Object = "envProtoClass"),
          .initialize_envProtoClass)
setMethod("initialize", signature(.Object = "protoCell"),
          .initialize_protoCell)
setMethod("initialize", signature(.Object = "protoContext"),
          .initialize_protoContext)
setMethod("initialize", signature(.Object = "protoContainer"),
          function(.Object, parentContainer = NULL, ...){
              ## tothink: .self is necessary?
              if(is.null(parentContainer)) parentContainer <- emptyenv()
              .Object <- callNextMethod()
              .Object@.xData <- new.env(TRUE, as.environment(parentContainer))
              .Object
          })
## initializeRoot
setGeneric("initializeRoot", function(.Object, ...) standardGeneric("initializeRoot"))
setMethod("initializeRoot", "envProtoClass",
          .initializeRoot_envProtoClass)
setMethod("initializeRoot", "protoCell",
          .initializeRoot_protoCell)
setMethod("initializeRoot", "protoContext",
          .initializeRoot_protoContext)

###__ setAs
setAs("environment", "envProtoClass", function(from){
    from <- callNextMethod()
    print("COOOl")
    from
})

###__ SHOW
setMethod("show", signature(object = "protoContext"),
          .show_Context)
setMethod("show", signature(object = "protoContainer"),
          .show_Container)
setMethod("show", signature(object = "envProtoClass"),
          .show_EnvProtoObject)
setMethod("show", signature(object = "protoFormWithEnv"),
          .show_ProtoFormWithEnv)
setMethod("print", signature(x = "protoFormWithEnv"),
          .print_ProtoFormWithEnv)
## protoContext and all inhereted protoContext classes are defined by contextClassRepresentation

###__ CLONE
setGeneric("clone",
           def = function(x, ...) standardGeneric("clone"),
           useAs = function(x, ...) x)
setMethod("clone", "envProtoClass",
          .clone_envProtoClass)
setMethod("clone", "protoContainer",
          .clone_protoContainer)
setMethod("clone", "cellContainer",
          .clone_cellContainer)
setMethod("clone", "protoContext",
          .clone_protoContext)

###__ DOLLAR
setMethod("$",
          signature(x = "envProtoClass"),
          definition = .dollarGet_envProtoClass
          )
setMethod("$",
          signature(x = "protoContext"),
          definition = .dollarGet_protoContext
          )
setMethod("$<-",
          signature(x = "envProtoClass"),
          definition = .dollarSet_envProtoClass
          )
setMethod("$<-",
          signature(x = "protoContext"),
          definition = .dollarSet_protoContext
          )

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
