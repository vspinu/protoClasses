protoClasses_debug_mode <- TRUE

if(existsMethod("initialize", "envProtoClass"))
    removeMethod("initialize", "envProtoClass")
if(existsMethod("initialize", "protoContext"))
    removeMethod("initialize", "protoContext")
if(existsMethod("initialize", "protoCell"))
    removeMethod("initialize", "protoCell")

###_ GENERIC METHODS:
setGeneric("installBinding",
           def = function(bindDefinition, container, bindName, ...) standardGeneric("installBinding"),
           useAsDefault = .installBinding_default)

setMethod("installBinding", "protoCell",
          function(bindDefinition, container, bindName, ...){
              ## bindName has precedence:
              if(!missing(bindName) && !identical(bindName, ""))
                  assign("type", bindName, envir = bindDefinition)
              else
                  bindName <- get(".type", envir = bindDefinition)
              .installCellInContainer(bindDefinition, container = container)
          })

setGeneric("initializeRoot", function(.Object, ...) standardGeneric("initializeRoot"))
setGeneric("clone",
           def = function(x, ...) standardGeneric("clone"),
           useAs = function(x, ...) x)


###_ FUNDAMENTAL OBJECTS:

###__ DEFINITIONS:
###___ protoCellDefinition
setClass("protoCellDefinition",
         representation(cellClass = "character",
                        names = "character"),
         prototype(cellClass = "protoCell"),
         contains = "namedList")

setMethod("installBinding", "protoCellDefinition",
          .installBinding_protoCellDefinition)

###___ protoFormDefinition
setClass("protoFormDefinition",
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


protoField <- function(func = function(value) NULL ,  doc = "", ...){
    subfunc <- substitute(func)
    if(protoClasses_debug_mode && is.name(subfunc) && is.function(func))
        func <- eval(substitute(
          function(value){
              loc_func <- func_name
              environment(loc_func) <- .self
              loc_func(value)
          }, list(func_name = subfunc)))
    new("protoField", func, doc = doc, ...)
}

protoReadOnlyField <- function(object_name)
    protoField(eval(substitute(
      function(obj){
          if(missing(obj)){
              get(obj_name, .self)
          }else{
              stop("field '", obj_name, "' is readonly")
          }
      }, list(obj_name = object_name))))

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
         representation(environment = "environment", form_name = "character"),
         contains = "protoForm") ## used for printing, $ methods returns this object with environment slot set to .self

setMethod("show", signature(object = "protoFormWithEnv"),
          function(object) .print_ProtoFormWithEnv(object, code = TRUE))

setMethod("print", signature(x = "protoFormWithEnv"),
          function(x, code = TRUE, ...){
              .sub <- function(te){
                  for( i in seq_along(te)){
                      ## if( class(te)=="{")
                      ##     te[[1]] <- .sub(te[[1]])
                      if (is.recursive(te) && typeof(te) != "special"){
                          src <- getSrcref(te)
                          if(isECall(te[[i]])){
                              fname <- as.character(te[[i]][[2]])
                              if(length(expand <- get(fname, where)) &&
                                 is.null(accum[[fname]])){
                                  accum[[fname]] <- 1
                                  expand <- .sub(expand)[[1]] ## get rid of "expression"
                                  subst <- list(e = te[[i]],
                                                file = paste0(
                                                  "defined in ", .getType(.get_form_host(where, fname)),
                                                  ' (from ', utils::getSrcFilename(expand),
                                                  "#", utils::getSrcLocation(expand,"line")[[1]], ")"))
                                  ## if(class(expand) == "{")
                                  ##     tte <- c(expand[[1]], substitute(e(file), subst), expand[-1])
                                  ## else{
                                  tte <- substitute({e(file);expand}, subst)
                                  tte[[3]] <- expand ## preserve srcref
                                  te[[i]] <- tte
                              }
                          } else if (is.recursive(te[[i]]))
                              te[[i]] <- .sub(te[[i]])
                      }}
                  te
              }

              accum <- new.env()
              where <- x@environment
              ## first one is alwyas recursive expression
              cat(paste0("## ", x@form_name,  " defined in [",
                         .getType(.get_form_host(where, x@form_name)), "] (from ",
                         getSrcFilename(x[[1]]), "#", getSrcLocation(x[[1]])[[1]], ")\n"))
              out <- .sub(x@.Data)
              ## names(out) <- NULL
              out <- capture.output(print(out))
              cat(gsub("e[(](.*).\"(.*)\"", "## e(\\1 \\2", out), sep = "\n")
          })

###___ protoFormWithBrowser
setClass("protoObjectWithBrowser",
         representation(original = "ANY",
                        hostEnv = "environment",
                        isInHost = "logical"))
setClass("protoFormWithBrowser",
         contains = c("protoForm", "protoObjectWithBrowser"))
setClass("protoMethodWithBrowser",
         contains = c("protoMethod", "protoObjectWithBrowser"))
setClass("protoFieldWithBrowser",
         contains = c("protoField", "protoObjectWithBrowser"))

###__ SETS of proto objects
## setClass("protoSET", representation(environment = "environment"), contains = "namedList")
## setClass("protoFormSET", contains = "protoSET")
## setClass("protoFieldSET", contains = "protoSET")
## setClass("protoMethodSET", contains = "protoSET")
## setClass("protoCellSET", contains = "protoSET")



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
          function(object) print(object))

setMethod("print", signature(x = "envProtoClass"),
          function(x, verbose = FALSE){
              object <- x
              cat("Proto Object of class \"", class(x),"\" ", format(as.environment(x)), "\n\n", sep = "")
              ## Show the context in the future  here (todo)
              objEnv <- as.environment(object)
              cat(" Type: \"", .getType(object), "\"\n", sep = "")
              cat(" Is Root: ", isRoot(object), "\n")
              cat(" Is Mirror: ", isMirror(object), "\n")
              VL <- 50
              bar <- "\t--------------------------------  \n"
              if(verbose){
                  methods:::.printNames("All objects: ", ls(objEnv, all.names = TRUE))
                  cat(" Containers:\n")
                  cat(" \n+ Fields:", bar)
                  str(.get_all_names_with_host(object[[".fields"]]), vec.len = VL)
                  ## print(.infoContainer(.get_all_names(objEnv[[".fields"]]), objEnv, ".fields"))
                  cat(" \n+ Methods:", bar)
                  str(.get_all_names_with_host(object[[".methods"]]), vec.len = VL)
                  ## print(.infoContainer(.get_all_names(objEnv[[".methods"]]), objEnv, ".methods"))
                  cat(" \n+ Forms:", bar)
                  str(.get_all_names_with_host(object[[".forms"]]), vec.len = VL)
              }
              ## for forms look in objEnv directly:
              ## print(infoForms(objEnv))
              ## str(list(Fields =  .get_all_names(objEnv[[".fields"]]),
              ##          Methods = .get_all_names(objEnv[[".methods"]]),
              ##          Forms = .get_all_names(objEnv[[".forms"]])),
              ##     no.list = TRUE, vec.len = 20, give.head = FALSE)
              ## methods:::.printNames("Fields: ", ls(objEnv[[".fields"]], all.names = TRUE))
              ## methods:::.printNames("Methods: ", ls(objEnv[[".methods"]], all.names = TRUE))
              ## methods:::.printNames("Forms: ", ls(objEnv[[".forms"]], all.names = TRUE))
          })

setMethod("clone", "envProtoClass", .clone_envProtoClass)

##' Dollar accessors of envProtoClasses
##'
##' "$" syntax is a multifunctional accessor for \code{envProtoClasses}
##' todo:....
##' @rdname dollar
##' @param x an object extending envProtoClass
##' 
setMethod("$",
          signature(x = "envProtoClass"),
          definition = .dollarGet_envProtoClass)


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

setMethod("initializeRoot", "protoContext",
          .initializeRoot_protoContext)
setMethod("show", signature(object = "protoContext"),
          function(object){
              callNextMethod()
              objEnv <- as.environment(object)
              VL = 50
              bar <- "\t--------------------------------  \n"
              cat("\n+ Cells:", bar)
              ## str(.get_all_names_with_host(".cells", objEnv), vec.len = VL)
              cell_names <- ls(objEnv[[".cells"]], all.names = TRUE)
              rev_names <- strsplit(cell_names, ".", fixed = TRUE)
              rev_names <- sapply(rev_names, function(el) paste(rev(el), collapse = "."))
              print(data.frame(` ` = cell_names[order(rev_names)], check.names = FALSE))
              cat("\n")
          })
setMethod("clone", "protoContext",
          .clone_protoContext)


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


