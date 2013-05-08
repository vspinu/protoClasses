###_ UTILS

.setType <- function(object, type){
    if(is(object, "protoCellDefinition"))
        object[["type"]] <- type
    else if (is(object, "protoCell"))
        assign(".type", type, envir = object)
    else
        stop("can not set the type for object of class \"", class(object), "\"")
    object
}

.getType <- function(object, fullName = T, collapse = ".", base_only = FALSE){
    if(is(object, "protoCellDefinition")) return(object[["type"]])
    if(is.null(object)) return("NULL")
    ## if(!is(object, "envProtoClass"))
    ##     stop("The 'object' argument must be of class 'protoCellDefinition' or 'envProtoClass',  suplied an object of class ",  class(object))
    object <- as.environment(object)
    ## fixme: simplify these two similar cases
    type_local <- function(x){
        x <- as.environment(x)
        if(isMirror(x))
            if(base_only) Recall(x[[".prototype"]])
            else c(x[[".type"]], Recall(x[[".prototype"]]))
        else if(isRoot(x) || !fullName) x[[".type"]]
        else c(x[[".type"]], Recall(x[[".prototype"]]))
    }
    if(is.character(collapse)) paste(type_local(object), collapse = collapse)
    else type_local(object)
}

.getPrototype <- function(cell)
    as.environment(cell)[[".prototype"]]

.insertSpecial <- function(objEnv, self, prototype){
    ## SPECIAL OBJECTS present in every envProtoObject:
    ## FUNCTIONS:
    e = eval(substitute(function(expr, type = "--")
      .Internal(eval(expr, envir, envir)), list(envir = objEnv)))
    environment(e) <- objEnv
    objEnv[["e"]] <- e
    objEnv[[".protozize"]] <- .protozize
    environment(objEnv[[".protozize"]]) <- objEnv
    objEnv[[".PROTOZIZE"]] <- .PROTOZIZE
    environment(objEnv[[".PROTOZIZE"]]) <- objEnv
    objEnv[[".cloneExclude"]] <- c()
    objEnv[[".cloneFirst"]] <- list()
    objEnv[[".cloneLast"]] <- list()
    objEnv[[".self"]] <- self
    objEnv[[".prototype"]] <- prototype
}

isValidProtoObject <- function(object, trigger_error = FALSE, object_name = "Object"){
    if(!inherits(object, "envProtoClass"))
        stop("Object is not of class 'envProtoClass'. Suplied object's class: '", class(object),"'")
    ## this sucks:
    checks <-
        c(
          `Empty environments are not allowed` = !identical(emptyenv(), as.environment(object)),
          `.prototype object not found` = exists(".prototype", envir = as.environment(object), inherits = FALSE),
          `.self object not found`  = exists(".self", envir = as.environment(object), inherits = FALSE),
          `.fields object not found` = exists(".fields", envir = as.environment(object), inherits = FALSE),
          `.methods object not found` = exists(".methods", envir = as.environment(object), inherits = FALSE),
          `.forms object not found` = exists(".forms", envir = as.environment(object), inherits = FALSE)
          )
    if(any(failed <- !checks)){
        if(trigger_error) stop(object_name, " is not a valid protoObject;",
                               "checks failed with the message(s) \n",
                               paste(names(checks)[failed], collapse = "\n"))
        FALSE
    }else{
        TRUE
    }
}



###_ CLASS

##' Parent S4 class of all proto objects.
##'
##' \link{protoContext-class} and \link{protoCell-class}
##' @export
setClass("envProtoClass", contains = "environment")

setAs("environment", "envProtoClass", function(from){
    from <- callNextMethod()
    from
})

setMethod("show", signature(object = "envProtoClass"),
          function(object) print(object))

## setMethod("print", signature(x = "envProtoClass"),
print.envProtoClass <- function(x, verbose = FALSE){
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
          }

##' Dollar accessors of envProtoClasses
##'
##' "$" syntax is a multifunctional accessor for \code{envProtoClasses}
##' todo:....
##' @rdname dollar
##' @param x an object extending envProtoClass
##' 
setMethod("$", signature(x = "envProtoClass"),
          function(x, name){
              selfEnv <- as.environment(x)
              out <- .getMethod(name, selfEnv)
              ## using missing() to allow storage of any object (i.e. NULL) in protoObjects
              if(missing(out)){
                  out <- .getField(name, selfEnv)
                  if(missing(out)){
                      out <- .getForm(name, selfEnv)
                      if(missing(out))
                          stop("Cannot find object \"", name, "\" in the protoObject of type ", .getType(x))
                  }
              }
              out
          })

##'
##' @rdname dollar
setMethod("$<-", signature(x = "envProtoClass"),
          function(x, name, value){
              out <- .setMethod(x, name, value, error=FALSE)
              if(missing(out)){
                  out <- .setField(x, name, value, error = FALSE)
                  if(missing(out)){
                      out <- .setForm(x, name, value, error = FALSE)
                      if(missing(out))
                          stop("\"", name, "\" is not a valid object in the protoObject of type '", .getType(x), "'")
                  }
              }
              invisible(x)
          })



###_ CONTAINERS
setClass("protoContainer",
         representation = list(typeContainer = "character", host = "envProtoClass"),
         contains = "environment")

setMethod("$", signature(x = "protoContainer"),
          definition = function(x, name) get(name, envir = x))

setMethod("show", signature(object = "protoContainer"),
          function(object){
              ## callNextMethod()
              cat(gettextf("A container of class \"%s\"\n", class(object)))
              methods:::.printNames("Contains: ", .get_all_names(object))
          })

setMethod("names",
          signature(x = "protoContainer"),
          function (x) {
              .get_all_names(x)
          })


## setMethod("length",
##           signature(x = "protoContainer"),
##           function (x) {
##               length(.get_all_names(x))
##           })

          
.get_all_names <- function(container){
    "Search recursively for names in 'container', returns all names."
    containerEnv <- as.environment(container)
    exclude <- specialNames(container)
    all_names <- c()
    while(!identical(containerEnv, emptyenv())){
        all_names <- c(all_names, ls(containerEnv, all.names = TRUE))
        containerEnv <- parent.env(containerEnv)
    }
    all_names <- unique(all_names)
    all_names[!(all_names %in% exclude)]
}

.get_all_names_with_host <- function(container){
    "Search recursively for names in 'container', return a list of the form
list(typeA = c('foo', 'bar'), typeB = ...)"
    host <- as.environment(container@host)

    if(is.character(container))
        container <- host[[container]]
    exclude <- specialNames(container)
    containerEnv <- as.environment(container)
    all_names <- list()
    while(!identical(containerEnv, emptyenv())){
        these_names <- ls(containerEnv, all.names = TRUE)
        all_names[[host[[".type"]]]] <- unique(these_names[!(these_names %in% exclude)])
        containerEnv <- parent.env(containerEnv)
        host <- as.environment(host)[[".prototype"]]
    }
    all_names
}

setMethod("initialize", signature(.Object = "protoContainer"),
          function(.Object, ...){
              .Object <- callNextMethod()
              parentContainer <- 
                  if(is.null(.Object@host) || is.null(prot <- .getPrototype(.Object@host))){
                      emptyenv()
                  }else{
                      prot[[.Object@typeContainer]]
                  }
              env <- new.env(TRUE, as.environment(parentContainer))
              .Object@.xData <- env
              .Object
          })



###_ CLASS REPRESENTATION
..eloadE0 <- expression(assignClassDef("envProtoClass", .modifyAs(getClassDef("envProtoClass"))))


###_ INITIALIZE
..eloadE1 <- expression({
    setMethod("initialize", signature(.Object = "envProtoClass"),
              function(.Object,
                       prototype = newRoot("envProtoClass"), ## tothink: what a heck is this here?
                       type = "--",
                       initMethods = list(), initFields = list(), initForms = list(),
                       setMethods = list(), setFields = list(), setForms = list(),
                       expr = expression(),
                       changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE), ...){

                  .Object <- callNextMethod()

                  ## !!!!! NO CLONING,  ALWAYS A NEW OBJECT !!!!!!!! ##
                  objEnv <- .Object@.xData <- new.env(TRUE)

                  ## BASIC VALIDATION:
                  if(!is(prototype, "envProtoClass")) # tothink: prototype should be from the same class as .Object??
                      stop("Class of prototype argument must extend \"envProtoClass\".\n Got an object of class \"", class(prototype), "\"")
                  isValidProtoObject(prototype, trigger_error = TRUE)
                  parent.env(objEnv) <-
                      protoEnv <- as.environment(prototype)


                  ## SPECIALS
                  .insertSpecial(objEnv, self = .Object, prototype = prototype)

                  ## FUNDAMENTAL CONTAINERS:
                  objEnv[[".fields"]] <- new("fieldContainer",  host = .Object)
                  objEnv[[".methods"]] <- new("methodContainer", host = .Object)
                  objEnv[[".forms"]] <- new("formContainer",  host = .Object)

                  ## DEFAULTS
                  .setField(.Object, "type", type) # type field was initialized  in the root
                  
                  ## USER supplied INITS
                  if(changeCallEnv){
                      initFields <- eval(substitute(initFields), envir = objEnv)
                      initMethods <- eval(substitute(initMethods), envir = objEnv)
                      initForms <- eval(substitute(initForms), envir = objEnv)
                  }
                  .initFields(initFields, .Object)
                  .initMethods(initMethods, .Object)
                  .initForms(initForms, .Object)
                  if(length(setFields))
                      .generic_setter(setFields, .Object, ".fields")
                  if(length(setMethods))
                      .generic_setter(setMethods, .Object, ".methods")
                  if(length(setForms))
                      .generic_setter(setForms, .Object, ".forms")
                  eval(expr, envir = objEnv)
                  .Object
              })
})


setMethod("initializeRoot", "envProtoClass",
          function(.Object,  ##TODO: !! get the "pure" initialization functionality into separate slot "initialize" in class definition!!
                   initForms = list(),
                   initFields = list(),
                   initMethods = list(),
                   type = "*",
                   ...){ #... not used here pu
              ## initialize the basic functionality for the ROOT object
              ## .fields, .prototype, basic methods etc
              objEnv <- as.environment(.Object)
              .signAsRoot(objEnv)
              parent.env(objEnv) <- topenv()  ## tohink: should be the namespace of protoClasses package ?
              ## tothink: lock these fields? 
              objEnv[[".fields"]] <- new("fieldContainer", host = .Object) ## emptyenv as parent by default
              objEnv[[".methods"]] <- new("methodContainer", host = .Object)
              objEnv[[".forms"]] <- new("formContainer", host = .Object)
              ## SPECIALS
              .insertSpecial(objEnv, self = .Object, prototype=NULL)
              ## objEnv[[".root"]] <- .Object

              ## BASIC FIELDS
              .initFields(list(type = protoField(
                                 function(value){
                                     if(missing(value))
                                         .type
                                     else{
                                         if(grepl(".", value, fixed = TRUE)){
                                             warning("\".\" was replaced with \"_\" in ", value)
                                             value <- gsub(".", "_", value, fixed = TRUE)
                                         }
                                         assign(".type", value, .self)
                                     }
                                 }),
                               Type = protoField(
                                 function(value){
                                     if(missing(value))
                                         protoClasses:::.getType(.self)
                                     else stop("Cannot assign extended type.")
                                 }),
                               proto = protoField(
                                 function(value){
                                     if(missing(value))
                                         .prototype
                                     else stop("Cannot reasign prototype.")
                                 }),
                               methods = protoContainerField(".methods"),
                               fields = protoContainerField(".fields"), 
                               forms = protoContainerField(".forms")), 
                          where = objEnv)
              
              .setField(objEnv, "type", type)

              ## BASIC METHODS
              .initMethods(list(
                new =
                function(type = "--", initMethods = list(),
                         initFields = list(), initForms = list(),
                         setMethods = list(), setFields = list(), setForms = list(),
                         expr = expression()){
                    new(class(.self), type = type, prototype = .self,
                        initMethods = initMethods, setMethods = setMethods,
                        initFields = initFields, setFields = setFields,
                        initForms = initForms, setForms = setForms,
                        expr = expr)
                }, 
                initMethods = function(..., .list = list(), changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)){
                    dots <-
                        if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                        else  c(list(...), .list)
                    protoClasses:::.initMethods(dots, .self)
                },
                setMethods = function(..., .list = list(), changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)){
                    dots <-
                        if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                        else c(list(...), .list)
                    protoClasses:::.generic_setter(dots, .self, ".methods")
                },
                initFields = function(..., .list = list(), .classes = list(), changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)) {
                    dots <-
                        if(changeCallEnv){
                            .classes <- eval(substitute(.classes), envir = .self)
                            eval(substitute(c(list(...), .list)), envir = .self)
                        }else  c(list(...), .list)
                    protoClasses:::.initFields(dots, .self, .classes)
                },
                setFields = function(..., .list = list(), changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)){
                    dots <-
                        if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                        else c(list(...), .list)
                    protoClasses:::.generic_setter(dots, .self, ".fields")
                },
                initForms = function(..., .list = list(), after = NULL,  changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)) {
                    dots <-
                        if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                        else c(list(...), .list)
                    protoClasses:::.initForms(dots, .self, after = after)
                },
                setForms = function(..., .list = list(), changeCallEnv = getOption("protoClasses.changeCallEnv", FALSE)){
                    dots <-
                        if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                        else c(list(...), .list)
                    protoClasses:::.generic_setter(dots, .self, ".forms")
                },
                debug = function(..., .methods, .fields, .forms){
                    .debugObjects(list(...), .methods = c(), .fields = c(), .forms = c(), .where = .self)
                }, 
                undebug = function(..., .methods = c(), .fields = c(), .forms = c(), .where = .self){
                    .undebugObjects(list(...), .methods, .fields, .forms, .where = .self)
                },
                inspect = function()
                eval(substitute({browser(skipCalls = 2);browser(skipCalls = 2)}), envir = .self), 
                expr = function(expr){
                    invisible(eval(substitute(expr), envir = .self))
                }),where = objEnv)
              
              ## "USER" FIELDS:
              .initFields(initFields, .Object)
              .initMethods(initMethods, .Object)
              .initForms(initForms, .Object)
              .Object
          })

eval(..eloadE0)
eval(..eloadE1)
evalOnLoad(..eloadE0)
evalOnLoad(..eloadE1)
