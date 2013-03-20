##' The \code{protoClasses} package is an implementation of
##' \href{http://en.wikipedia.org/wiki/Prototype-based_programming}{prototype} programming paradigm.
##'
##'
##' Classes to support prototype-style classes with reference-semantics for
##' fields, object-based methods, forms and cells.
##'
##' The implementation is a cross between \code{proto-package} and R's \code{refClasses}. 
##'
##' The "envProtoClass" is the basic class from which protoContext and protoCell
##' are inhereted.
##' @name protoClasses-package
##' @docType package
##' @title Prototype based programming with context and forms.
##' @author Vitalie Spinu \email{spinuvit@@gmail.com}
##' @keywords package prototype
NULL
.self <- NULL

options(protoClasses = list(changeCallEnv = FALSE))
## if TRUE  eval(substitute(list(...)) gets rid of source code !!!
## fixme: !!

##' Create a class definition of a cell.
##'
##' \code{setCellClass} differs from \code{setClass} only in that class
##' representation contains an additional slot \code{contextClass} to represent
##' the default class of the cell's context.
##' @param Class class name
##' @param contextClass a string representing the context class with which this
##' cell class is associated.
##' @param contains what classes does this class extend. "protoCell" class is
##' added automatically to this list.
##' @param where see \code{\link{setClass}}
##' @param ... other parameters to \code{\link{setClass}}
##' @seealso \link{protoCell}, \link{protoContext}, \link{setContextClass}
##' @author Vitalie Spinu
##' @export
setCellClass <- function(Class,
                         contextClass = "protoContext",
                         contains = character(),
                         where = topenv(parent.frame()), ##todo: provide "initialize" slot here!!
                         ...){
    if(!any(unlist(sapply(contains, extends, "protoCell"))))
        contains <- unique(c(contains, "protoCell"))
    ## contains <- unique(c(contains, "protoCell")) <- messes up the hierarchy
    setClass(Class, contains = contains,
             where = where, ...)
    classDef <- new("cellClassRepresentation",
                    getClassDef(Class, where = where),
                    contextClass = contextClass)
    classDef <- .modifyAs(classDef)
    assignClassDef(Class, classDef)
    Class
}


##' Create a class definition for a proto context.
##'
##' Proto contexts are main objects envProtoClass objects which allow building
##' hierarchical inheritance in prototype OO programming. ProtoContext objects
##' inherit form other protoContext objects in the sense that all methods,
##' fields, forms and cells contained in parent object are also visible in the
##' children contexts unless overwritten.
##' @param Class  class name
##' @param defaultContext an object inherited from \code{protoContext}. This is
##' the context from which all the children contexts of the current class will
##' inherit (in the sense of the prototype inheritance).
##' @param cellClass class of the cells which populate this context class
##' @param contains what classes this class extend. In addition to the default
##' \code{protoContext} class.
##' @param where where to install the class definition
##' @param ... other arguments to \link{setClass}
##' @seealso protoCell, protoContext, setContextClass
##' @author Vitalie Spinu
##' @export
##'
setContextClass <- function(Class,
                            defaultContext = new("protoContext", type = paste(Class, "@",  sep = "")), ## not root by default
                            cellClass = "protoCell",
                            contains = character(),
                            where = topenv(parent.frame()),
                            ...){
    ## temporarily assign an ordinary class definition
    if(!any(unlist(sapply(contains, extends, "protoContext"))))
        contains <- unique(c(contains, "protoContext"))
    setClass(Class, contains = contains,
             where = where, ...)
    ## generate contextClassRepresentation definition
    classDef <- new("contextClassRepresentation",
                    getClassDef(Class, where = where),
                    defaultContext = defaultContext,
                    cellClass = cellClass)
    classDef <- .modifyAs(classDef)
    ## assign the new definition
    assignClassDef(Class, classDef, where, force = force)
    classDef@defaultContext <- as(defaultContext, Class) ## default context should be also of class "Class"
    assignClassDef(Class, classDef, where, force = force)
    .signAsDefaultContext(defaultContext)
    Class
}

.modifyAs <- function(classDef){
    .coerce <-  classDef@contains[[1]]@coerce
    body(.coerce) <- bquote({value <- .(body(.coerce))
                             assign(".self", value, envir = value)
                             value})
    classDef@contains[[1]]@coerce <- .coerce
    ## have to modify all he replace methods:
    for(i in seq_along(classDef@contains)){
        .replace <- classDef@contains[[i]]@replace
        body(.replace) <- bquote({from <- .(body(.replace))
                                  assign(".self", from, envir = from)
                                  from})
        classDef@contains[[i]]@replace <- .replace
    }
    classDef
}

###_ INTERNAL FUNCTIONS
## Rely only on internal implementation.
## Always use as.environment (do not rely on [[]], or other user changeable methods)
## All this funcs start with .
## Do not rely on  objects methods (like $, methods, etc) (only func programming)

###_* META NAMES
.rootMetaName <- ".|.root"
.defaultMetaName <- ".|.defaultContext"


##' Return TRUE if envProtoObj is a root object
##'
##' @param envProtoObj Object from a subclass of envProtoClass
##' @author Vitalie Spinu
##' @export
isRoot <- function(envProtoObj)
    exists(.rootMetaName, envir = as.environment(envProtoObj), inherits = FALSE)

.signAsRoot <- function(envProtoObj)
    assign(.rootMetaName, TRUE, envir = as.environment(envProtoObj), inherits = FALSE)

##' Return TRUE is it's a default context
##'
##' @param envProtoObj Object from a subclass of envProtoClass
##' @author Vitalie Spinu
isDefaultContext <- function(envProtoObj)
    exists(.defaultMetaName, envir = as.environment(envProtoObj), inherits = FALSE)

.signAsDefaultContext <- function(envProtoObj)
    assign(.defaultMetaName, TRUE, envir = as.environment(envProtoObj), inherits = FALSE)


###_* INITIALIZE METHODS
## ROOT
newRoot <- function(Class, ...){
    ClassDef <- getClass(Class, where = topenv(parent.frame()))
    .Object <- .Call("R_do_new_object", ClassDef, PACKAGE = "base")
    .Object@.xData <- new.env(TRUE, parent = topenv(parent.frame()))
    initializeRoot(.Object, ...)
}


###_ + envProtoClass
.insertSpecial <- function(objEnv, self, prototype){
    ## SPECIAL OBJECTS present in every envProtoObject:
    ## FUNCTIONS:
    e = eval(substitute(function(expr)
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

create_specialised_accesor <- function(type){
    fun <- eval(substitute(
                  function(value){
                      if(missing(value))
                          container_name
                      else{
                          if(!is(value, "environment") || !identical(as.environment(container_name), as.environment(value)))
                              warning("Oops, trying to assing non-native container. Are you messing with internals through user interface?")
                      }
                  }, list(container_name = as.name(type))))
    attributes(fun) <- NULL
    fun
}

###_ envProtoClass
.initializeRoot_envProtoClass <- function(.Object,  ##TODO: !! get the "pure" initialization functionality into separate slot "initialize" in class definition!!
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
                           else assign(".type", .replaceDots(as.character(value)), .self)
                       }),
                     Type = protoField(
                       function(value){
                           if(missing(value))
                               .getType(.self)
                           else stop("Cannot assign extended type.")
                       }),
                     m = protoField(create_specialised_accesor(".methods")),
                     f = protoField(create_specialised_accesor(".fields")),
                     h = protoField(create_specialised_accesor(".forms"))),
                where = objEnv)
    .setField(objEnv, "type", type)

    ## BASIC METHODS
    .initMethods(list(
                   initMethods = function(..., .list = list(), changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                           else  c(list(...), .list)
                       .initMethods(dots, .self)
                   },
                   setMethods = function(..., .list = list(), changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                           else c(list(...), .list)
                       .generic_setter(dots, .self, ".methods")
                   },
                   initFields = function(..., .list = list(), .classes = list(), changeCallEnv = getOption("protoClasses")$changeCallEnv) {
                       dots <-
                           if(changeCallEnv){
                               .classes <- eval(substitute(.classes), envir = .self)
                               eval(substitute(c(list(...), .list)), envir = .self)
                           }else  c(list(...), .list)
                       .initFields(dots, .self, .classes)
                   },
                   setFields = function(..., .list = list(), changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                           else c(list(...), .list)
                       .generic_setter(dots, .self, ".fields")
                   },
                   initForms = function(..., .list = list(), after = NULL,  changeCallEnv = getOption("protoClasses")$changeCallEnv) {
                       dots <-
                           if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                           else c(list(...), .list)
                       .initForms(dots, .self, after = after)
                   },
                   setForms = function(..., .list = list(), changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(c(list(...), .list)), envir = .self)
                           else c(list(...), .list)
                       .generic_setter(dots, .self, ".forms")
                   },
                   methods = function(..., changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else  list(...)
                       .generic_getter(dots, .self, ".methods")
                   },
                   fields = function(..., changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else  list(...)
                       .generic_getter(dots, .self, ".fields")
                   },
                   forms = function(..., changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else  list(...)
                       .generic_getter(dots, .self, ".forms")
                   },
                   expr = function(expr){
                       invisible(eval(substitute(expr), envir = .self))
                   }),
                 where = objEnv)


    ## "USER" FIELDS:
    .initFields(initFields, .Object)
    .initMethods(initMethods, .Object)
    .initForms(initForms, .Object)
    .Object
}

## .dummy_envProtoObject <- newRoot("envProtoClass")
.initialize_envProtoClass <-
    function(.Object,
             prototype = newRoot("envProtoClass"), ## tothink: what a heck is this here?
             type = "--",
             initMethods = list(), initFields = list(), initForms = list(),
             setMethods = list(), setFields = list(), setForms = list(),
             expr = expression(),
             changeCallEnv = getOption("protoClasses")$changeCallEnv, ...){

        .Object <- callNextMethod()

        ## !!!!! NO CLONING,  ALWAYS A NEW OBJECT !!!!!!!! ##
        objEnv <- .Object@.xData <- new.env(TRUE)

        ## BASIC VALIDATION:
        if(!is(prototype, "envProtoClass")) # tothink: prototype should be from the same class as .Object??
            stop("Class of prototype argument must extend \"envProtoClass\".\n Got an object of class \"", class(prototype), "\"")
        isValidProtoObject(prototype,trigger_error = TRUE)
        parent.env(objEnv) <-
            protoEnv <- as.environment(prototype)


        ## SPECIALS
        .insertSpecial(objEnv, self = .Object, prototype = prototype)

        ## FUNDAMENTAL CONTAINERS:
        objEnv[[".fields"]] <- new("fieldContainer",  host = .Object)
        objEnv[[".methods"]] <-new("methodContainer", host = .Object)
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
    }

###_ + protoContext
.initializeRoot_protoContext <- function(.Object, rootParentEnv = globalenv(), ##fixme:  make field:
                                         initCells = list(), ...){
    .Object <- callNextMethod(.Object, ...) ##envProtoClass
    objEnv <- as.environment(.Object)
    objEnv[[".cells"]] <-  new("cellContainer", host = .Object) ## empty parent by default
    objEnv[[".rootCellParentEnv"]] <- rootParentEnv # parent of the .rootCell
    .initMethods(list(initCells =
                      function(...){
                          dotsq <- substitute(list(...))
                          dots <- list(...)
                          for(i in seq_along(dots)){
                              ## infer the "type" of the cell from the names if type == "--"
                              if(is.name(dotsq[[i + 1L]]) && is(dots[[i]], "protoCell")&& .getType(dots[[i]]) == "--")
                                  ## names of ... have precedence over type!!
                                  ## can not touch them here!
                                  dots[[i]] <- .setType(dots[[i]], as.character(dotsq[[i + 1L]]))
                          }
                          .initCells(dots, .self)
                      },
                      cells = function(...){
                          selfEnv <- as.environment(.self)
                          .generic_setter(list(...), selfEnv, ".cells")
                      }),
                 where = objEnv)
    .Object
}


.initialize_protoContext <- function(.Object,
                                     prototype = NULL,
                                     initCells = list(),
                                     cells = list(),
                                     rootParentEnv = NULL, ## todo:  make field
                                     ...){
    ## PROTOTYPE must be a valid envProtoObject (todo: validate!!)
    if(is.null(prototype)){
        prototype <- getClassDef(class(.Object))@defaultContext
    }
    .Object <- callNextMethod(.Object, prototype = prototype, ...) # envProtoClass
    objEnv <- as.environment(.Object)

    ## Cells
    parent.cells <- get(".cells", envir = prototype, inherits = FALSE)
    objEnv[[".cells"]] <-  new("cellContainer", parentContainer = parent.cells, host = .Object)
    objEnv[[".self"]] <- .Object
    objEnv[[".rootCellParentEnv"]] <-
        if(is.null(rootParentEnv))
            get(".rootCellParentEnv", envir = objEnv[[".prototype"]])
        else rootParentEnv

    ## New Cells
    .initCells(initCells, .Object)
    if(length(cells))
        .generic_setter(cells, .Object, ".cells")
    .Object
}

###_ + protoCell
.initializeRoot_protoCell <- function(.Object,
                                      homeContext = NULL,  ...){
    .Object <- callNextMethod(.Object, ...) ##envProtoClass
    list2env(list(.homeContext = homeContext),
             envir = as.environment(.Object))
    .Object
}


.initialize_protoCell <- function(.Object,
                                  prototype = "*",
                                  type = "--",
                                  homeContext = NULL,
                                  ## initFields = list(), initMethods = list(), initForms = list(),
                                  ## fields = list(), methods = list(), forms = list(),
                                  ## expr = expression(),
                                  ...){
    ## PROTOTYPE can be a character string or a valid envProtoObject
    ## ---
    ## DON'T clone the CELL if CELL of type_long exists in the inherited container (why to clone?)
    ## create a new one otherwise.
    ## CELL is *NOT* installed in the context (it's the task of initCell interface)
    ## note: cannot create the root cell!! supplying * as TYPE produces the cell *.*

    ## CONTEXT
    context <-
      if(is.null(homeContext))
        ##default context
        getClassDef(getClassDef(class(.Object))@contextClass)@defaultContext
    else as.environment(homeContext)[[".self"]]
    
    isValidProtoObject(context, trigger_error = TRUE, object_name = "context")
    ## PROTOTYPE
    .cells <- as.environment(get(".cells", context))
    if(is.character(prototype)){
        prototype <- .getPartial(prototype, container = .cells, trigger_error = TRUE,
                                 object_name = "prototype")
    }
    ## CHECK for the validity of the CELL and TYPE:
    isValidProtoObject(prototype, trigger_error = TRUE, object_name = "prototype")
    if(!is(context, tCls <- getClassDef(class(.Object))@contextClass))
        stop(gettextf("Attempt to create an object of class %s in a homeContext of class %s which doesn't extend cell's default context class %s",
                      class(.Object), class(homeContext), tCls))

    ## CREATE THE CELL
    type <- as.character(type)
    ## type_long <- paste(type, .getType(prototype), sep = ".")
    ## the_cell <-
    ## if(is.null(homeContext)){ # default context
    ##     ## lookup in DEFAULT .cells!!
    ##     if(exists(type_long, envir = .cells))
    ##         get(type_long, envir = .cells)
    ##     else NULL
    ## }else{
    ## lookup in the PARENT of .cells
    ## if(exists(type_long, envir = parent.env(.cells)))
    ##     get(type_long, envi = parent.env(.cells))
    ## else NULL
    ## }

    ## if(!exists(type_long, envir = parent.env(.cells))){
        ## create a NEW one:
    .Object <- callNextMethod(.Object, type = type, prototype = prototype,  ...)
    ## }else{
    ##     ## type_long is FOUND so  CLONE:
    ##     the_cell <- get(type_long, envi = parent.env(.cells))
    ##     obj <- as.environment(clone(the_cell))
    ##     ## REDIRECT parents of containers ## note: no problems with cloning * cell; it's just not allowed to create *
    ##     parent.env(get(".forms", envir = obj)) <- get(".forms", envir = prototype)
    ##     parent.env(get(".methods", envir = obj)) <- get(".methods", envir = prototype)
    ##     parent.env(get(".fields", envir = obj)) <- get(".fields", envir = prototype)
    ##     .Object@.xData <- as.environment(obj) ##!! do not replace the incoming object class!!!
    ## }

    ## ## Special OBJECTS:
    objEnv <- as.environment(.Object)
    objEnv[[".homeContext"]] <- homeContext
    objEnv[[".self"]] <- .Object

    ## User INITS:
    ## .initFields(initFields, .Object)
    ## .initMethods(initMethods, .Object)
    ## .initForms(initForms, .Object)
    ## if(length(fields))
    ##     .generic_setter(fields, .Object, ".fields")
    ## if(length(methods))
    ##     .generic_setter(methods, .Object, ".methods")
    ## if(length(forms))
    ##     .generic_setter(forms, .Object, ".forms")
    ## eval(expr, envir = objEnv)
    .Object
}

###_* FUNDAMENTAL OBJECTS
###_ + FORMS
.makeEexpr <- function(formName)
    substitute(e(formName), list(formName = as.name(formName)))

.makeEForm <- function(formName){
    ## protoForm of the form form(cc = e(aa.bb.cc))
    short_name <- strsplit(formName, ".", fixed = TRUE)[[1L]]
    short_name <- short_name[[length(short_name)]]
    expr <- expression()
    expr[[short_name]] <- .makeEexpr(formName)
    new("protoForm",  expr)
}

.makeNames <- function(names){
    ## forms must have non-empty names (used in initialization for "protoForm")
    if(length(names)){
        if(is.null(names)||is.na(names))
            names <- rep.int("", length(names))
        if(!all(empty <- nzchar(names)))
            names[!empty] <- array(LETTERS, dim = sum(!empty))
        make.unique(names, sep = "")
    }else{
        character()
    }
}

form <- function(..., doc = character()){
    ## FIXME:  substitute(expression(...)) does not preserve the source
    ## it looks like there is no other way but to copy the implementation of
    ## expression() function which is a primitive.
    ## For now F() incapsulation does the job.

    expr <- substitute(expression(...))
    if(length(expr) > 1L){
        for(i in 2:length(expr)){
            sym <- expr[[i]]
            if(is.recursive(sym) && (sym[[1]] == as.name(".F") ||
                                     sym[[1]] == as.name("form") ||
                                     sym[[1]] == as.name("as.form"))){
                expr[[i]] <- eval(expr[[i]])
            }
        }
    }
    new("protoForm", eval(expr), doc = doc)
}

.F <- function(expr, doc = character()){
    ## force evaluation of encapsulated expressions F.(...), form(), as.forms in
    ## order to generate subexpressions that are actually forms and not just
    ## expressions.
    pframe <- parent.frame()
    ## expr <- eval(substitute(expression(...)))
    if(missing(expr)) expr <- expression()
    for(i in seq_along(expr)){
        sym <- expr[[i]]
        if(is.recursive(sym) && (sym[[1]] == as.name(".F") ||
                                 sym[[1]] == as.name("form") ||
                                 sym[[1]] == as.name("as.form"))){
            expr[[i]] <- eval(expr[[i]], pframe)
        }
    }
    new("protoForm", expr, doc = doc)
}

as.form <- function(from){
    new("protoForm", as.expression(from))
}

## as.form(as.expression(c(expression(232 - 342), Bbb = quote(343 - 3))))

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

## SetsClass("protoECall",
##          representation(formName = "character"),
##          prototype(formName = "<undefined_protoForm>"),
##          contains = "call")

## setMethod("initialize", "protoECall",
##           function(.Object, ...){
##               .Object <- callNextMethod()
##               .Object@.Data <-
##                   substitute(e(formName),
##                              list(formName = as.name(.Object@formName)))
##               .Object
##           })

## removeMethod("initialize", "protoECall")
isECall <- function(obj){
    if(is.recursive(obj) && (typeof(obj) != "special"))
        identical(obj[[1L]], as.name("e"))
    else
        FALSE
}

## .getOrCreateForm <- function(bindName, whereEnv)
##     if(exists(bindName, envir = get(".forms", envir = whereEnv))){
##         get(bindName, envir = whereEnv)
##     }else{
##         new("protoForm")
##     }


.replaceDots <- function(names){
    if(length(dot_here <- grep(".", names, fixed = TRUE))){
        warning("\".\" was replaced with \"_\" in ",
                paste(names[dot_here], collapse=", "))
        names <- gsub(".", "_", names, fixed = TRUE)
    }
    names
}

.assignSubForms <- function(formName, form, where, register = TRUE, after = NULL){
    " If oldForm with the name formName exists replace supbforms form FORM in oldForm,
assign form with formName otherwise.
 If AFTER is NULL just replace the subforms from old form by new subforms (the default)
 if <= 0 - preppend, if => length(oldForm) append. No name checks.
 Don't assign if new form is identical to the installed one."
    do_assign <- FALSE
    if(exists(formName, envir = get(".forms", envir = where))){
        ## adds forms to existing one
        oldForm <- get(formName, envir = where)
        if(!is.null(after)){
            if(is.character(after))
                after <- match(after, names(oldForm))
            else
                after <- as.integer(after)
            if(is.na(after))
                ## if not found by match or bigger number than length just append
                after <- NULL
        }
        newForm <-
            if(is.null(after)){
                newForm <- oldForm
                for(nm in allNames(form))
                    newForm[[nm]] <- form[[nm]] ## subforms should replace the old ones
                newForm
            }else if (after >= length(oldForm)){
                as(c(oldForm, form), class(oldForm))
            }else if (after <= 0){
                as(c(form, oldForm), class(oldForm))
            }else{
                as(c(oldForm[1L:after], form, oldForm[(after + 1L):length(oldForm)]), class(oldForm))
            }
        if(!identical(newForm, oldForm))
            do_assign <- TRUE
    }else{
        newForm <- form
        do_assign <- TRUE
    }
    if(do_assign){
        assign(formName, newForm, envir = where)
        .installBinding_default(new("protoFormDefinition", formClass = class(newForm)),
                                container = where[[".forms"]], formName)
    }
    do_assign
}

.installBinding_protoForm <- function(bindDefinition, container, bindName,
                                      after = NULL, returnBinding = bindName,
                                      reinstal = FALSE){
    ## * Used recursively:
    ## if bindDefinition is a form - assign in the whereEnv and return e(...)
    ## if bindDefinition is an expression - just return (as it is called recursively)
    ## * main idea: an assigned form should have only calls like e(aa.bb.cc) or
    ## simple expressions. No forms as children.
    ## * If the form's name is like aa.bb.cc ,check for existence of aa.bb and aa
    ## and install the eform e(aa.bb.cc) into aa.bb with the name "cc" and
    ## e(aa.bb) into "aa" with the name "bb".
    ## * If bindDefinition == NULL then this form ("aa.bb") and all the children forms
    ## ("aa.bb.cc.dd" etc) are removed from CONTAINER
    ## * if AFTER is nonnull it must be character or an integer specifying
    ## the position of the new form "cc" in the parent form "aa.bb"

    whereEnv <- as.environment(container@host)
    form <- bindDefinition

    ## bindName is aa.bb.cc
    rec_names <- unlist(strsplit(bindName, split = ".",  ## "aa" "bb" "cc"
                                 fixed = TRUE), use.names=FALSE)
    cum_names <- Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
                        c(paste(x[[1]], y, sep = "."), x), rec_names)
    if(is.null(form))
        return(.removeFormWithChildren(bindName, whereEnv))

    if(is(form, "protoForm")){
        ## create parent forms (i.e. aa.bb, aa) if needed:
        assigned <- TRUE
        last_assigned <- bindName
        while(assigned && length(cum_names) > 1L){
            assigned <-
                .assignSubForms(cum_names[[2]], ## aa.bb = form(cc = e(aa.bb.cc))
                                 .makeEForm(cum_names[[1]]), whereEnv, after = after)
            last_assigned <- cum_names[[1L]]
            cum_names <- cum_names[-1L]
        }
        ## print(last_assigned)
        shortNames <- names(form) ## (dd, ff)
        firstNames <- gsub("\\..*", "", shortNames)
        longNames <- paste(bindName, shortNames, sep = ".") ## (aa.bb.cc.dd, aa.bb.cc.ff)
        newForm <-
            if(!reinstal && exists(bindName, envir = whereEnv[[".forms"]]))
                get(bindName, envir = whereEnv)
            else new("protoForm")
        stopifnot(is(newForm, "protoForm"))
        for(i in seq_along(firstNames)){
            if(!is.null(newForm[[firstNames[[i]]]]))
                stop.pbm("subexpression `", firstNames[[i]], "` in form `", bindName,
                         "` is already initialised. Use 'removeForm' first, or 'setForms' instead", env = whereEnv)
        }
        for(i in seq_along(firstNames)){
            newForm[[firstNames[[i]]]] <-
                ## if a form then install, if expression, just return
                .installBinding_protoForm(
                  form[[i]], container, longNames[[i]],
                  returnBinding = paste(bindName, firstNames[[i]], sep = "."))
        }
        # -------------------------------------------------------------- #
        # install newForm only if different                              #
        # if(exists(bindName, envir = get(".forms", envir = whereEnv))){ #
        #     oldForm <- get(bindName, envir = whereEnv)                 #
        # if(!identical(newForm, oldForm)){                              #
        # -------------------------------------------------------------- #
        ## todo: get the followining into unit test somehow
        if(exists(bindName, whereEnv) && 
           (ln <- length(setdiff(names(get(bindName, whereEnv)), names(newForm)))))
            warn.pbm("warning: ", ln, " subforms removed in '", bindName, "' form", env = whereEnv)
        assign(bindName, newForm, envir = whereEnv)
        .installBinding_default(new("protoFormDefinition", formClass = class(form)),
                                container, bindName, ".forms")
        return(.makeEexpr(returnBinding))
    }else{
        ## .assignForm should no be used directly to assign non protoForm objects !!
        return(form)  ## returns as is (i.e. expression), nothing is assigned
    }
}

.initForms <- function(forms, where, after = NULL, emptyforms = c()){
    "init the FORMS in the object WHERE"
    whereEnv <- as.environment(where)
    ## tothink: do I really need emptyforms?
    if(length(emptyforms) > 0L){
        forms <- c(forms, setNames(as.list(rep(expression(), length(emptyforms))), as.character(emptyforms)))
    }
    if(!is.null(after))
        forms <- rev(forms)
    ## non empty names?
    formNames <- names(forms)
    if(length(forms)>0 && ( is.null(formNames) ||
                           !all(nzchar(formNames))))
        stop("Arguments to 'initForms' must have nonempty names")
    if(!(all(which <- unlist(lapply(forms, function(x) is.language(x) || is.null(x))))))
        stop("Arguments to 'initForms' must be a subclass of name, call or expression /see ?is.language/. Not true for ", paste(formNames[!which], collapse = ", "))
    ## look for objects to remove (new definition is NULL)
    removeThese <- sapply(forms, is.null)
    if(any(removeThese)){
        remFormNames <- formNames[removeThese]
        lapply(remFormNames, .removeFormWithChildren, selfEnv = whereEnv)
    }
    ## convert all to protoForm objects
    forms <- lapply(forms, function(el) as(el, "protoForm"))
    ## install all bindings and update the container
    formNames <- names(forms)
    for(i in seq_along(forms))
        installBinding(forms[[i]], whereEnv[[".forms"]], formNames[[i]], after = after)
    invisible(formNames)
}


###_ + ACCESSES
.generic_setter <- function(dots, .self, container_name){
    ## selfEnv <- as.environment(.self)
    switch(container_name,
           .fields = {setFUN <- .setField},
           .forms = { setFUN <- .setForm},
           .methods = { setFUN <- .setMethod},
           .cells = { setFUN <- .setCell},
           stop("Unrecognized setter type ", container_name))
    if(length(dots) == 0L)
        warning("No arguments to setter (", container_name, "); nothing assigned")
    else if(all(nzchar(names <- allNames(dots)))){
        ## SET named objects (as side effect)
        lapply(names, function(nm)
               setFUN(.self, nm, dots[[nm]]))
        invisible(names)
    }else{
        stop("Supplied empty names to the setter (", container_name, ")")
    }
}

.generic_getter <- function(dots, .self, container_name){
    selfEnv <- as.environment(.self)
    switch(container_name,
           .fields = {getFUN <- .getField},
           .forms = {getFUN <- .getForm},
           .methods = {getFUN <- .getMethod},
           .cells = {getFUN <- .getCell},
           stop("Unrecognized setter type", container_name))
    .extract <-
        function(names, selfEnv){
            lapply(names, function(nm){
                out <- getFUN(nm, selfEnv)
                if(missing(out))
                    stop("Can not find object \"", nm, "\"")
                else
                    out
            })
        }
    if(length(dots) == 0){
        ## GET all names
        names <- .get_all_names(get(container_name, envir = selfEnv))
        setNames(.extract(names, selfEnv), names)
    }else{
        names <- unlist(dots, TRUE, FALSE)
        if(!all(sapply(names, is.character)))
            stop("Accessor accepts only character vectors or lists of character vectors.")
        if(!all(nzchar(names)))
            stop("Accessor accepts only nonempty names")
        setNames(.extract(names, selfEnv), names)
    }
}


###_  * GETTERS
## .existsMethod <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".methods"]])
## }
.dollarGet_methodContainer <- function(x, name){
    meth <- get(name, envir = x)
    environment(meth) <- x@host
    if(meth@changeCallEnv) meth <- x@host[[".PROTOZIZE"]](meth)
    meth
}
.getMethod <- function(name, selfEnv){
    if(exists(name, envir = selfEnv[[".methods"]])){
        .dollarGet_methodContainer(selfEnv[[".methods"]], name)
    }else{
        substitute()
    }
}

## .existsField <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".fields"]])
## }

.dollarGet_fieldContainer <- function(x,name){
    field_fun <- get(name, envir = x)
    environment(field_fun) <- x@host
    field_fun()
}
.getField <- function(name, selfEnv, error = FALSE)
    if(exists(name, envir = selfEnv[[".fields"]])){
        .dollarGet_fieldContainer(selfEnv[[".fields"]], name)
    }else{
        substitute()
    }


## .existsForm <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".forms"]])
## }
.dollarGet_formContainer <- function(x, name)
  get(name, envir = x) ## fixme: what should this do? now it return a string 

.getForm <- function(name, selfEnv)
    if(exists(name, envir = selfEnv[[".forms"]])){
        ## .dollarGet_formContainer(selfEnv[[".forms"]], name)
        new("protoFormWithEnv", get(name, envir = selfEnv),
            environment = selfEnv, form_name = name)
    }else{
        substitute()
    }
    

## .existsCell <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".cells"]])
## }
## .existsPartialCell <- function(name, selfEnv){
##     .existsPartial(name, selfEnv[[".cells"]])
## }

.dollarGet_cellContainer <- function(x, name)
    .getPartial(name, x)
.getCell <- function(name, selfEnv){
    .cells <- as.environment(selfEnv[[".cells"]])
    .getPartial(name, .cells, trigger_error = FALSE)
}

.getPrototype <- function(cell)
    as.environment(cell)[[".prototype"]]
.setHomeContext <- function(cell, context)
    assign(".homeContext", context, as.environment(cell))


###_  * SETTERS
## SET new value only if the object already exists in the hierarchy
.dollarSet_fieldContainer <- function(x, name, value, error = TRUE){
    ## x is a container
    if(exists(name, envir = x)){
        field_fun <- get(name, envir = x)
        environment(field_fun) <- x@host
        field_fun(value) ## side effect here (must assign to environment)
        return(x)
    }else{
        if(error) stop("Object \"", name, "\" is not a valid field in the protoObject of type \"", .getType(x@host), "\"")
        else substitute()
    }
}

.setField <- function(x, name, value, error = TRUE)
    .dollarSet_fieldContainer(get(".fields", envir = x), name, value, error)

.dollarSet_methodContainer <- function(x, name, value, error = TRUE)
    ## x is a container
    if(exists(name, envir = x)){
        ## oldMeth <- get(name, envir = x) ;; need to compare with an old one really?  probably not
        if(!is.function(value))
            stop("Methods must be functions. Not true for '", name, "'")
        method <- new("protoMethod", value)
        installBinding(method, x, name)
        return(x)
    }else{
        if(error) stop("Object \"", name, "\" is not a valid method in the protoObject of type \"", .getType(x@host), "\"")
        else substitute()
    }

.setMethod <- function(x, name, value, error = TRUE)
    ## x is protoEnv
    .dollarSet_methodContainer(get(".methods", envir = x), name, value, error)

.dollarSet_formContainer <- function(x, name, value, error = TRUE, after = NULL){
    if(exists(name, envir = x) &&
       is(oldForm <- get(name, envir = x@host), "protoForm")){
        is_eCall <- unlist(sapply(oldForm, isECall))
        shortNames <- names(oldForm)
        longNames <- paste(name, shortNames, sep = ".")
        ##         lapply(seq_along(names(oldForm)[is_eCall]), function(nm){
        ##             ## shit!! FIXME:: TODO: does not work for complex forms
        ##             ## if use '!is(value[[nm]], "protoForm") && ' in check the protoForm
        ##             ## goes to instalation and installes subforms even if they are not present yet!!
        ##             if(!identical(oldForm[[nm]], value[[nm]]))
        ##                 stop("Can only modify non-form elements with \"setForms\" interface.
        ## Not true for element \"", longNames[[nm]], "\"
        ## Use \"initForms\" to install new forms.")
        ##         })
        value <- as(value, "protoForm")
        if(!identical(oldForm@.Data, value@.Data)){
            .removeFormWithChildren
            .installBinding_protoForm(value, x, name, reinstal = TRUE)
        }
        return(x)
    }else{
        if(error) stop("Object ", name,
                       " is not a valid form in the protoObject of type \"",
                       x@host$type, "\"\nUse `initForms` to initialize new forms")
        else substitute()
    }
}

.setForm <- function(x, name, value, after = NULL, error = TRUE){
    .dollarSet_formContainer(get(".forms", envir = x), name, value, error, after)
}

.setCell <- function(x, name, value, error = TRUE){
    stop("Not implemented yet")
}

###_  * KILLERS
.removeFormWithChildren <- function(name, selfEnv){
    "removes recursively registred forms.
If name is aa.bb all the registered forms starting with aa.bb (inclusive) will
 be removed from selfEnv and selfEnv[['.forms']] container."
    contEnv <- get(".forms", envir = selfEnv)
    all_names <- .get_all_names(contEnv)
    to_remove <- grep(name, all_names, value = T, fixed = T)
    to_remove <- to_remove[substring(to_remove, 1, nchar(name)) == name]
    ## unlist is neede when to_remove is character(0)
    to_remove_container <- to_remove[unlist(sapply(to_remove, exists, envir = contEnv, inherit = FALSE))]
    to_remove_self <- to_remove[unlist(sapply(to_remove, exists, envir = selfEnv, inherit = FALSE))]
    remove(list = to_remove_container, envir = as.environment(contEnv))
    remove(list = to_remove_self, envir = as.environment(selfEnv))
    invisible(NULL)
}

.emptyFormWithChildren <- function(name, selfEnv){
    ## tothink: do we need this?
    "Emptyfy recursively registred forms.
If name is aa.bb, all the registered forms starting with aa.bb will
 be emptyfied (i.e. assigned .F()) in selfEnv."
    contEnv <- get(".forms", envir = selfEnv)
    all_names <- .get_all_names(contEnv)
    ## FIXME: need only at the beggingin of the string.
    to_empty <- grep(name, all_names, value = T, fixed = T)
    to_empty <-
        to_empty[substring(to_empty, 1, nchar(name)) == name]
    ## unlist is needed in case to_empty is character(0)
    lapply(to_empty, assign,  value = .F(), envir = selfEnv)
    invisible(NULL)
}


###_  * DOLLAR
.dollarGet_envProtoClass <- function(x, name){
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
}

.dollarSet_envProtoClass <- function(x, name, value){
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
}

.dollarGet_protoContext <- function(x, name){
    if(!is.null(obj <- .getCell(name, as.environment(x))))
        return(obj)
    else
        .dollarGet_envProtoClass(x, name)
}

.dollarSet_protoContext <- function(x, name, value){ # must follow dollar arguments, dont realy like this :(
    .cells <- get(".cells", envir = x)
    match <- .complete_partial_name(name, .cells)
    if(is(value, "protoCell") && !(is.na(match) || match == 0L)){
        assign(match, value, envir = .cells)
    }else{
        .dollarSet_envProtoClass(x, name, value)
    }
    invisible(x)
}


###_ + FIELDS
.initialize_Field <- function(.Object,  ...){
    .Object <- callNextMethod()
    name <- .Object@bindName
    className <- .Object@className
    if(is.null(body(.Object)) && length(className) > 0L){
        ## Default fields (i.e. function was not supplied)
        metaName <- as.character(name)
        if(identical(className, "ANY")){
            ## "Any"
            .Object@.Data <-
                eval(substitute(function(value) {
                    if(missing(value)){
                        dummyField
                    }else{
                        assign(dummyName, value, envir = .self)
                        invisible(value)
                    }
                }, list(dummyField = as.name(metaName),
                        dummyName = metaName)))
        }else if(isVirtualClass(className)){
            ## "VIRTUAL"
            .Object@.Data  <-
                eval(substitute(function(value){
                    if(missing(value)){
                        dummyField
                    }else{
                        if(is(value, dummyClass)){
                            ## value <- as(value, dummyClass)
                            assign(dummyName, value, envir = .self)
                            invisible(value)
                        }
                        else
                            stop(gettextf("Field %s must be of class \"%s\"; supplied an object of class \"%s\"",
                                          substitute(dummyField), dummyClass, class(dummyField)))
                    }
                }, list(dummyField = as.name(metaName),
                        dummyName = metaName,
                        dummyClass = className)))
        }else if(extends(className, "function")){
            .Object@.Data <-
                eval(substitute(function(value) {
                    if(missing(value)){
                        dummyField
                    }else{
                        value <- as(value, dummyClass)
                        environment(value) <- .self
                        assign(dummyName, value, envir = .self)
                        invisible(value)
                    }
                }, list(dummyField = as.name(metaName),
                        dummyName = metaName,
                        dummyClass = className)))
        }else{
            ## all other classes
            .Object@.Data <-
                eval(substitute(function(value) {
                    if(missing(value)){
                        dummyField
                    }else{
                        value <- as(value, dummyClass)
                        assign(dummyName, value, envir = .self)
                        invisible(value)
                    }
                }, list(dummyField = as.name(metaName),
                        dummyName = metaName,
                        dummyClass = className)))
        }
    }
    ## .Object@className <- if(length(className) == 0L)  "custom" else className
    .Object
}

.initFields <- function(fields, where, classes = list()){
    "Install the FIELDS in the object WHERE"
    ## can supply NULL to fields or "NULL" class to remove
    fieldClasses <- character()
    fieldInits <- list()
    fieldNames <- character()
    if(length(classes) > 0L){
        classes <- unlist(classes)
        ## classes or NULL
        fieldNames <- names(classes)
        if(is.null(fieldNames) || !all(nzchar(fieldNames)))
            stop("Classes argument to initFields must have nonempty names")
        if(!all(sapply(classes, is.character)))
            stop("Classes argument to initFields must be a character vector or list of strings.")
        fieldClasses <- as.character(classes) ## converts NULL to "NULL":)
        fieldInits <- sapply(fieldClasses, function(class)
                             if(isVirtualClass(class)) "NA" else new(class))
    }
    ## TREAT AS A LIST OF INITIAL VALUES
    fieldNames1 <- names(fields)
    if( length(fields)> 0 && (is.null(fieldNames1) ||
                              !all(nzchar(fieldNames1))))
        stop("Arguments to initFields must have nonempty names")
    fieldNames <- c(fieldNames, fieldNames1)
    fieldClasses <- c(fieldClasses, lapply(fields,  class))
    fieldInits <- c(fieldInits, fields)

    whereEnv <- as.environment(where)
    ## look for objects to remove (new definition is NULL)
    removeThese <- sapply(fieldClasses, identical, "NULL")
    if(any(removeThese)){
        rfieldNames <- fieldNames[removeThese]
        fieldNames <- fieldNames[!removeThese]
        fieldClasses <- fieldClasses[!removeThese]
        fieldInits <- fieldInits[!removeThese]
        .removeFromContainer(names = rfieldNames, ".fields", whereEnv)
    }
    ## install all bindings in the container .fields
    for(i in seq_along(fieldNames)){
        field <-
            if(extends(fieldClasses[[i]], "protoField")){
                ## protoField is suplied; don't assign initial value in WHERE
                new("protoField", fieldInits[[i]], bindName = fieldNames[[i]])
            }else{
                if(!isVirtualClass(fieldClasses[[i]]))
                    assign(fieldNames[[i]], fieldInits[[i]], envir = whereEnv)
                new("protoField",
                    bindName = fieldNames[[i]],
                    className = fieldClasses[[i]])
            }
        installBinding(field, whereEnv[[".fields"]], fieldNames[[i]])
    }
    invisible(fieldNames)
}

###_ + METHODS
.initMethods <- function(methods, where){
    "Install the methods in the object WHERE"
    ## methods is an list which is received as dots in the wrapper.
    ## check for  non empty names
    methodNames <- names(methods)
    if(length(methods)> 0 && (is.null(methodNames) || !all(nzchar(methodNames))))
        stop("Arguments to initMethods() must be named")
    if(any(not_fun <- sapply(methods, function(m) !(is.function(m)||is.null(m)))))
        stop("Suplied objects /", paste(names(methods)[not_fun], collapse = ", "), "/ are not functions")
    ## look for objects to remove (new definition is NULL)
    whereEnv <- as.environment(where)
    removeThese <- sapply(methods, is.null)
    if(any(removeThese)){
        rmethodNames <- methodNames[removeThese]
        methodNames <- methodNames[!removeThese]
        methods <- methods[!removeThese]
        .removeFromContainer(names = rmethodNames, ".methods", where)
    }
    ## install in container
    for(i in seq_along(methods)){
        method <- new("protoMethod", methods[[i]])
        installBinding(method, whereEnv[[".methods"]], methodNames[[i]])
    }
    invisible(methodNames)
}

###_ + CELLS
.initCells <- function(cells, where){
    "Install CELLS in the object WHERE"
    ## cells must be a list,  names(cells) have precedence over internal type;
    ##  'character' elements are looked up in the .cells environment,  if cannot
    ##  find produce an error!! do not overwrite names in the program!
    ##
    if(!is(where, "protoContext")) stop("Argument \"where\" must extend the class \"protoContext\"")
    if(is.list(cells)) {
        ## can have empty names, will be taken from type.
        cellTypes <- allNames(cells)
        if(!all(sapply(cells, function(el)
                       is.null(el) ||
                       is(el, "protoCell") ||
                       is(el, "protoCellDefinition") ||
                       is(el, "character")))) ## must be one of existing cells! don't create a new cell!!
            stop("Argument for initCells must be of class  \"protoCell\" , \"protoCellDefinition\" or \"character\" vector of existing types")
    }
    else
        stop(gettextf("cells arguement must must be a list got an object of class \"%s\"",
                      class(cells)), domain = NA)
    ## look for objects to remove (new definition is NULL)
    whereEnv <- as.environment(where)
    removeThese <- sapply(cells, is.null)
    if(any(removeThese)){
        remCellNames <- cellTypes[removeThese]
        cellTypes <- cellTypes[!removeThese]
        cells <- cells[!removeThese]
        .removeFromContainer(names = remCellNames, ".cells", where)
    }
    for(i in seq_along(cells)){
        if(is.character(cells[[i]]))
            ## install from inhereted contexts ## canot use 'new', it can not produce "*" cell
            cells[[i]] <- .getPartial(cells[[i]],
                                      get(".cells", envir = whereEnv), object_name = "cell") # error if not found
        ##names have precedence over types here (types should not be used explicitly at user level)
        installBinding(cells[[i]], whereEnv[[".cells"]], cellTypes[[i]])
    }
    invisible(cellTypes)
}

.installCellInContainer <- function(cell, container){
    ## CELL and PROTOTYPES are cloned if not homeless or not already  associated
    ## with CONTEXT.
    ## CELL and missing PROTOTYPES are inserted into the container (i.e. prototype
    ## chain is followed as long as proto_type is not found in current context).
    ## return an installed (might be the same) cell
    .to_clone <- function(cell, context)
        ## CLONE if not homeless and is not associated with the CONTEXT
        !(is.null(homeContext(cell)) || identical(as.environment(context),
                                                  as.environment(homeContext(cell))))
    stopifnot(is(cell, "protoCell"))
    context <- as.environment(container@host)[[".self"]] ## fixme: S4 class of container@host is disincronised with .self
    if(!is(context, tCls <- getClassDef(class(cell))@contextClass))
        stop(gettextf("Suplied context class /%s/ does not extend the default context class of the cell /%s/",
                      class(context), tCls))
    contextEnv <- as.environment(context)
    if(.to_clone(cell, context))
        cell <- clone(cell)
    if(!extends(class(cell), tCls <- getClassDef(class(context))@cellClass))
        cell <- as(cell, tCls)  ## note: some functionality might be missing, provide explicit coerce method.
    containerEnv <- as.environment(container)
    if(identical("--", as.environment(cell)[[".type"]]))
        stop("Cannot install cell of type \"--\"; please supply the type argument.")

    ## KEEP CLONING and INSERTING  prototypes when not in the container
    cell_to_return <- cell
    prototype <- .getPrototype(cell)
    while(!is.null(prototype) &&
          !exists(prot_type <- .getType(prototype),
                  envir = containerEnv, inherits = FALSE)){
        containerEnv[[.getType(cell)]] <- cell
        .redirect_prototypes(cell, containerEnv)
        .setHomeContext(cell, context)
        setPrototype(cell, ## sets parent.env as well
                     if(.to_clone(prototype, context)) clone(prototype)
                     else prototype )
        ## ------
        cell <- .getPrototype(cell)
        prototype <- .getPrototype(cell)
    }
    ## if cell is already in container, nothing changes
    containerEnv[[.getType(cell)]] <- cell
    .redirect_prototypes(cell, containerEnv)
    .setHomeContext(cell, context)
    if(is.null(prototype)){ ## root
        cellEnv <- as.environment(cell)
        parent.env(cellEnv) <- get(".rootCellParentEnv", contextEnv)
    }else{
        ## Disregard the prototype if it's type already exists in the current container!!
        setPrototype(cell, containerEnv[[prot_type]])
    }
    invisible(cell_to_return)
}

.redirect_prototypes <- function(prototype, container){
    ##make all the cells which point to .getType(prototype) to point to new prototype
    prot_type <- .getType(prototype)
    lapply(ls(container, all.names = TRUE), function(nm){
        cell <- get(nm, envir = container, inherits = FALSE)
        if(identical(.getType(get(".prototype", envir = cell)), prot_type))
            setPrototype(cell, prototype)
    })
}

cellFromDefinition <- function(protoCellDefinition, homeContext){
    stopifnot(is(protoCellDefinition, "protoCellDefinition"))
    ## infer name! SIDE EFFECT (does not work here,  this func is called usually  internally)
    ## nameCell <- substitute(protoCellDefinition)
    ## if(is.name(nameCell) && identical(protoCellDefinition[["type"]], "--"))
    ##     protoCellDefinition[["type"]] <- as.character(nameCell)
    if(is(protoCellDefinition$prototype, "protoCellDefinition"))
        protoCellDefinition[["prototype"]] <- cellFromDefinition(protoCellDefinition$prototype, homeContext)
    protoCellDefinition[["homeContext"]] <- homeContext
    do.call("new", c(list(Class = protoCellDefinition@cellClass), protoCellDefinition))
}

homeContext <- function(cell)
    get(".homeContext", envir = cell)

C <- function(...){
    args <- list(...)
    args[["Class"]] <- NULL
    new("protoCellDefinition", args, cellClass = "protoCell")
}

.installBinding_default <- function(bindDefinition, container, bindName, ...){
    ## default methods just assigns the stuff in the container.
    assign(bindName, bindDefinition, envir = container)
}

.installBinding_protoCell <- function(bindDefinition, container, bindName, ...){
    ## bindName has precedence:
    if(!missing(bindName) && !identical(bindName, ""))
        assign("type", bindName, envir = bindDefinition)
    else
        bindName <- get("type", envir = bindDefinition)
    .installCellInContainer(bindDefinition, container = container)
}

.installBinding_protoCellDefinition <- function(bindDefinition, container, bindName, ...){
    if(!missing(bindName) && !identical(bindName, ""))
        bindDefinition[["type"]] <- bindName
    new_cell <- cellFromDefinition(bindDefinition, homeContext = container@host)
    installBinding(new_cell, container, bindName) ## fixme: bindName is not needed here?
    invisible(new_cell)
}

.installBinding_protoField <- function(bindDefinition, container, bindName, ...){
    ## assign if different
    ## containerEnv <- get(container, envir = where)
    ## if(exists(bindName, envir = containerEnv)){
    ##     oldField <- get(bindName, envir = containerEnv)
    ##     ## if(!identical(bindDefinition, oldField))
    ##     ##     callNextMethod()
    ##     ## ## else do nothing
    ## }else{
    callNextMethod()
}

.changeEnvFuncs <- function(envir){
    ## environments of ALL functions from envir is changed to ENVIR
    ## todo: make changeThisOnly argument
    envir <- as.environment(envir)
    all.names <- ls(envir, all.names = T)
    lapply(all.names, function(nm)
           if(is.function(envir[[nm]])) environment(envir[[nm]]) <- envir
           )
    invisible(NULL)
}

.changeEnvEnvs <- function(envir, changeThisOnly = NULL){
    ## parent environments of ALL environments from ENVIR is changed to ENVIR
    ## if changeThisOnly is an environment then only objects with parent.env ==
    ## changeThisOnly are changed.
    envir <- as.environment(envir)
    if(!(is(changeThisOnly, "environment") || is.null(changeThisOnly)))
        stop("changeThisOnly must be NULL or of subclass of 'environment', suplied an object of class ", class(changeThisOnly))
    all.names <- ls(envir, all.names =  T)
    lapply(all.names, function(nm)
           if(is(envir[[nm]], "environment" ) && (is.null(changeThisOnly) ||
                                                  identical(parent.env(envir[[nm]]), as.environment(changeThisOnly))))
           parent.env(envir[[nm]]) <- envir)
    invisible(NULL)
}

.changeContainersHosts <- function(host){
    envir <- as.environment(host)
    all_names <- ls(envir, all.names=T)
    sapply(all_names, function(nm)
           if(is(envir[[nm]], "protoContainer")) envir[[nm]]@host <- host)
}
    
###_  * CLONES(no side effects)
.clone_envProtoClass <- function(x, exclude_names = c(), ...){
    ## EXCLUDE_NAMES are not cloned or copied (ex. .cells in protoContext class)
    ## TOTHINK: environments of ALL functions is changed here to point to new environment
    y <- x
    xEnv <- as.environment(x)
    y@.xData <- yEnv <- new.env(TRUE, parent = parent.env(xEnv))
    lapply(xEnv[[".cloneFirst"]], eval, envir = yEnv)
    x_names <- ls(envir=xEnv, all.names=TRUE)
    exclude_names <- c(exclude_names, ".self", ".prototype", ".homeContext", 
                       as.character(xEnv[[".cloneExclude"]]))
    x_names <- x_names[!(x_names %in% exclude_names)]
    x_names <- x_names[!sapply(x_names, bindingIsActive, xEnv)] # active bindings not cloned
    prototype <- xEnv[[".prototype"]]
    if(!isRoot(x))
        isValidProtoObject(prototype, trigger_error = TRUE, object_name = "prototype")
                       lapply(x_names, function(nm) assign(nm,
                                        value=clone(xEnv[[nm]], ...),
                                        envir=yEnv))
    ## SPECIALS
    .insertSpecial(yEnv, self=y, prototype=prototype)
    yEnv[[".homeContext"]] <- xEnv[[".homeContext"]]
    if(isRoot(x)){
        ## yEnv[[".root"]] <- y
        yEnv[[".prototype"]] <- NULL ## just in case
    }

    .changeEnvFuncs(yEnv) ## fixme: should change environments only of those which point to xEnv!!!
    .changeEnvEnvs(yEnv, changeThisOnly = xEnv)
    .changeContainersHosts(y) ## make .self of containers to point to "y" object
    lapply(xEnv[[".cloneLast"]], eval, envir = yEnv)
    return(y)
}

.clone_protoContainer <- function(x, exclude_names = c(), ...){
    ## ATTENTION: !!!! cloning of containers will *not* change the parent of the
    ## clone, nor the .self it's your task to change that to point elsewhere in
    ## the program (so far only needed in cloning envProtoObjects
    exclude_names <- c(exclude_names, ".self")
    y <- x
    xEnv <- as.environment(x)
    y@.xData <- yEnv <- new.env(TRUE, parent = parent.env(x))
    x_names <- ls(envir=xEnv, all.names=TRUE)
    x_names <- x_names[!(x_names %in% exclude_names)]
    lapply(x_names,
           function(nm) assign(nm, value=clone(xEnv[[nm]], ...), envir=yEnv))
    y
}

.clone_cellContainer <- function(x, exclude_names = c(), ...){
    ## .cells are ???not???? cloned, a new container is always created
    ## tothink:might be necessary for complete replication
    y <- callNextMethod()
    cell_names <- ls(y, all.names = TRUE)
    ## redirect new chells to new prototypes
    lapply(cell_names, function(nm){
        if(!isRoot(y[[nm]])){
            tp <- .getType(y[[nm]][[".prototype"]])
            prot <- y[[tp]]
            if(!is.null(prot)){
                parent.env(y[[nm]]) <- y[[nm]][[".prototype"]] <- prot
            }else
                warning("cell container is not complete, prototype ", tp, " is not found")
        }})
    y
}


.clone_protoContext <-  function(x, exclude_names = c(), clone.cells = FALSE, ...){
    ## exclude_names <- unique(exclude_names, ".cells")
    y <- callNextMethod(x, exclude_names = exclude_names, ...)
    ## change homeContext of cells to y
    cells <- y[[".cells"]]
    cell_names <- ls(cells, all.names = TRUE)
    lapply(cell_names, function(nm){
        cells[[nm]][[".homeContext"]] <- y
    })
    y
}


###_* IDENTICAL?

## tdf <- as.data.frame(infoForms(pbm$GaNorm))
##  browseForms(pbm$GaNorm)
## highlight(output = "test.tex", parser.output = parser(text = deparse(lm)), renderer = renderer_latex(document = T))

## ## renderer_html()
## ## renderer
## setMethod("@<-")
## method.skeleton("@<-", "envProtoClass")
## setMethod("@<-",
##           signature(object = "envProtoClass"),
##           function (object, name, value)
##       {
##           .self <- get(".self", envir = object)
##           slot<-()
##       }
##           )

areIdentical <- function(c1, c2){
    ##  comapre two envProtoObjects
    reserved <- c( ".fields", ".forms", ".homeContext", ".methods", ".prototype", ".self",  ".cells", ".protozize", ".PROTOZIZE", "e")
    names1 <- ls(c1, all.names =  T)
    names2 <- ls(c2, all.names =  T)
    if(length(diff1 <- setdiff(setdiff(names1, names2), reserved)))
        message("folowing names are found in c1 and not c2: \n",
                paste(diff1, collapse = ", "))
    if(length(diff2 <- setdiff(setdiff(names2, names1), reserved)))
        message("folowing names are found in c2 and not c1: \n",
                paste(diff2, collapse = ", "))
    ## if(length(diff1) == 0 && length(diff2) == 0){
    diffs <- sapply(names <- setdiff(names1, reserved), function(nm) !isTRUE(all.equal(c1[[nm]], c2[[nm]])))
    if(any(diffs)){
        cat("Differ : \n", paste(names[diffs], sep = ", "), "\n")
        FALSE
    }else{
        TRUE
    }
    ## }
    ## else
    ##     TRUE
}

areIdenticalPBM <- function(pbm1, pbm2){
    names1 <- ls(pbm1[[".cells"]], all.names =  T)
    names2 <- ls(pbm2[[".cells"]], all.names =  T)
    reserved <- character()
    if(length(diff1 <- setdiff(setdiff(names1, names2), reserved)))
        message("folowing cells are found in pbm1 and  not in pbm2: \n",
                paste(diff1, collapse = ", "))
    if(length(diff2 <- setdiff(setdiff(names2, names1), reserved)))
        message("folowing cells are found in pbm2 and not in pbm1: \n",
                paste(diff2, collapse = ", "))
    if(length(diff1) == 0 && length(diff2) == 0){
        diffs <- sapply(names1, function(nm) {
            cat(" --> ", nm, "\n")
            areIdentical(pbm1[[".cells"]][[nm]], pbm2[[".cells"]][[nm]])
        })
        diffs
    }else{
        TRUE
    }
}


###_* SHOW
.show_ContextClassDef <- function(object){
    cat("Extended class definition (", methods:::classLabel(class(object)),
        ")\n")
    methods:::printClassRepresentation(object)
    ## Show the context in the future  here (todo)
    objEnv <- as.environment(object@defaultContext)
    cat("\nContainers in the default context:\n\n")
    str(list(Fields =  ls(objEnv[[".fields"]], all.names = TRUE),
             Methods = ls(objEnv[[".methods"]], all.names = TRUE),
             Forms = ls(objEnv[[".forms"]], all.names = TRUE)),
        no.list = TRUE)
}


.show_EnvProtoObject <- function(object){
    callNextMethod()
    ## Show the context in the future  here (todo)
    objEnv <- as.environment(object)
    cat(" Type: \"", .getType(object), "\"\n", sep = "")
    methods:::.printNames("All objects: ", ls(objEnv, all.names = TRUE))
    methods:::.printNames("Is Root: ", isRoot(object))
    VL <- 50
    bar <- "\t--------------------------------  \n"
    cat(" Containers:\n")
    cat(" \n+ Fields:", bar)
    str(.get_all_names_with_host(object[[".fields"]]), vec.len = VL)
    ## print(.infoContainer(.get_all_names(objEnv[[".fields"]]), objEnv, ".fields"))
    cat(" \n+ Methods:", bar)
    str(.get_all_names_with_host(object[[".methods"]]), vec.len = VL)
    ## print(.infoContainer(.get_all_names(objEnv[[".methods"]]), objEnv, ".methods"))
    cat(" \n+ Forms:", bar)
    str(.get_all_names_with_host(object[[".forms"]]), vec.len = VL)
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


.show_Context <- function(object){
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
}


.show_Container <- function(object){
    ## callNextMethod()
    cat(gettextf("A container of class \"%s\"\n", class(object)))
    methods:::.printNames("Contains: ", .get_all_names(object))
}

## .print_ProtoFormWithEnv <- function(x, code = TRUE, ...){
##     ## assumes that "name" exists and checks are not made.
##     .print_local <- function(name, where, lev, code){
##         if(exists(name, envir = where)){
##             form <- get(name, envir = where)
##             if(is(form, "protoForm")){
##                 cat(rep(".. ", lev),"e(", name, ")\n", sep = "")
##                 if(length(form@doc)) {
##                     cat("## DOC: \n")
##                     print(form@doc)

##                 }
##                 for(i in seq_along(form)){
##                     fm <- form[[i]]
##                     if(isECall(fm)){
##                         Recall(as.character(fm[[2]]), where, lev = lev + 1L, code)
##                     }else{
##                         if(code){
##                             te <- as.expression(fm)[[1]]
##                             attr(te, "wholeSrcref") <- NULL ## kludge
##                             attr(te, "srcfile") <- NULL ## kludge
##                             cat(paste(paste(rep.int("   ", lev), collapse = ""),
##                                       capture.output(print(te))),  sep ="\n")
##                         }}
##                 }
##             }else{
##                 cat(rep(".. ", lev),"e(", name, ") <-- missing\n", sep = "")
##             }
##         }}
##     ## cat("##", name, "\n", sep = "")
##     f_names <- names(x)
##     for(i in seq_along(x)){
##         fm <- x[[i]]
##         if(isECall(fm)){
##             .print_local(as.character(fm[[2]]), x@environment, lev = 0L, code)
##         }else{
##             cat(f_names[[i]], "\n")
##             te <- as.expression(fm)[[1]]
##             attr(te, "wholeSrcref") <- NULL ## kludge
##             attr(te, "srcfile") <- NULL ## kludge
##             print(te)
##         }
##     }
## }

.print_ProtoFormWithEnv <- function(x, code = TRUE, ...){
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
}


## te <- expression(if(ab) e(pp))
## pp <- expression(a + b)
## tsub(te)

.show_ProtoFormWithEnv <- function(object)
    .print_ProtoFormWithEnv(object, code = TRUE)

## .print_ProtoFormWithEnv(Mh$LUR$set.ll)
###_ + summaries

.summaryForms <- function(object){
    .forms <- get(".forms", envir = object)
    .infoContainer(.get_all_names(.forms), object, ".fields")
}


###_* UTILITIES
.insertBrowserInForm <- function(form){
    form_browser <- as(c(expression(browser = browser()), original = form),
                       "protoFormWithBrowser")
    form_browser@original <- as(as.expression(form), "protoForm")
    form_browser
}

.debugForm <- function(form_name, where){
    form <- .getForm(form_name, where)
    if(missing(form))
        stop("Form \"", form_name, "\" is not found in the cell of type \"", .getType(where), "\"")
    form_browser <- .insertBrowserInForm(form)
    form_browser@hostEnv <- where
    form_browser@isInHost <- exists(form_name, envir = where, inherits = FALSE)
    assign(form_name, form_browser, envir = where)
    invisible(form_name)
}

.undebugForm <- function(form_name, where){
    form <- .getForm(form_name, where)
    if(missing(form))
        stop("Form \"", form, "\" is not found in the cell of type \"", .getType(where), "\"")
    if(is(form, "protoFormWithBrowser")){
        remove(list = form_name, envir = form@hostEnv)
        if(form@isInHost)
            assign(form_name, form@original, envir = form@hostEnv)
    }else{
        stop("form \"", form_name, "\"is not marked for debugging")
    }
}


.complete_partial_name <- function(name, container){
    ## return the full name from the container
    ## NA if not found 0 if partial multiple match
    all_names <- .get_all_names(container)
    match <- charmatch(name, all_names)
    if(!(is.na(match)||match == 0L))
        all_names[[match]]
    else
        match
}

## fullType <- function(part_type, obj){
##     if(is(obj, "protoContainer"))
##         .complete_partial_name(part_type, container)
##     else if(is(obj, "protoContext"))
##         .complete_partial_name(part_type, get(".cells", envir = obj))
##     else
##         stop("obj must be of class protoContainer or protoContext,  suplied an object of class \"", class(obj), "\"")
## }


## .existsPartial <- function(name, container){
##     "TRUE if object with partial name NAME exists in the container"
##     all_names <- .get_all_names(container)
##     match <- charmatch(name, all_names)
##     if(is.na(match)||match == 0L)
##         FALSE
##     else
##         TRUE
## }

.getPartial <- function(name, container, trigger_error = TRUE, object_name = "object"){
    "Return an object with the partial name 'name' from container.
Return NULL if trigger_error = FALSE and match not found."
    all_names <- .get_all_names(container)
    match <- charmatch(name, all_names)
    if(!trigger_error && (is.na(match)||match == 0L))
        return(NULL)
    if(is.na(match))
        stop(gettextf("Can not find the %s with the (partial) name \"%s\"",
                      object_name, name))
    if(match == 0L)
        stop(gettextf("The name \"%s\" does not match uniquely the %ss' names ",
                      name, object_name))
    get(all_names[[match]], envir = container)
}

.removeFromContainer <- function(names, container, where){
    remove(list = names, envir = as.environment(where[[container]]))
}

## .first_protoObject <- function(envProtoObj, name = "*", container = ".cells"){
##     ## "Return the first envProtoObject in which \nthe NAME is installed in the CONTAINER "
##     ## env <- as.environment(envProtoObject)
##     ## if(exists(name, env[[container]])){
##     ##     while(!exists(name, env[[container]], inherits = FALSE))
##     ##         env <-  as.environment(get(".prototype", envir = env))
##     ##     return(invisible(env))
##     ## }else{
##     ##     return(invisible(NULL))
##     ## }
## }


.PROTOZIZE <- function(fun){
    ## not working, , , tothink:
    ## it'a a bulshit idea. Why do I need this? Forms are for this.
    "Protozize the function FUN by changing it's environment and
 argument evaluation environment to .self"
    mc <- match.call()
    mc <- as.list(mc[[2]])[-1]
    do.call(fun, mc, envir = as.environment(.self))
}

.protozize <- function(fun){
    "Protozize the function FUN by changing it's environment to .self"
    environment(fun) <- .self
    fun
}


.changedObjects <- function(env, regexp = NULL, names){
    ## prints the hierarchy of changed objects in teh parents of env
    ## ENV  is an envProtoObject
    tcollate <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", tcollate))
    Sys.setlocale("LC_COLLATE", "C")
    env <- as.environment(env)
    if(missing(names) && missing(regexp))
        regexp <- "*"
    if(!is.null(regexp)){
        tenv <- env
        all_names <- c()
        while(!(identical(tenv, emptyenv()) || is.null(tenv)) && exists(".type", envir = tenv)){
            all_names <- c(all_names, ls(tenv, all.names =  T))
            tenv <- parent.env(tenv)
        }
        names <- grep(regexp, unique(all_names), value = TRUE)
    }
    out <- data.frame(row.names = names)
    while(!(identical(env, emptyenv()) || is.null(env)) && exists(".type", envir = env)){
        names_in <- names %in% ls(env, all.names = TRUE)
        type <- get(".type", envir = env)
        out <- cbind(names_in, out)
        names(out)[[1]] <- type
        env <- parent.env(env)
    }
    iout <- as.matrix(out)
    out[iout] <- " +"
    out[!iout] <- "  "
    out <- out[order(row.names(out)),, drop = FALSE]
    predots <- unlist(lapply(strsplit(row.names(out), ".", fixed = T),
                             function(el) paste(rep(". ", length(el) - 1), collapse = "")))
    row.names(out) <- paste(predots, row.names(out), sep = "")
    out
}

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

.get_form_host <- function(cell, form_name){
    "Get the form's host cell. No checks. return empty env if not found"
    cellEnv <- as.environment(cell)
    while(!(exists(form_name, cellEnv, inherits = F) || identical(cellEnv, emptyenv()))){
        cellEnv <- parent.env(cellEnv)
    }
    cellEnv[[".self"]]
}


.infer_type_cell <- function(cell){
    " looks in the .type field if that is --, returns the symbol cell,
if not a symbol, returns NULL"
    stopifnot(is(cell, "protoCell"))
    name <- substitute(cell)
    cellEnv <- as.environment(cell)
    if(identical(cellEnv[[".type"]], "--"))
        if(is.name(name)) return(name)
        else return(NULL)
    else return(cellEnv[[".type"]])
}

## *******************
.setType <- function(object, type){
    if(is(object, "protoCellDefinition"))
        object[["type"]] <- type
    else if (is(object, "protoCell"))
        assign(".type", type, envir = object)
    else
        stop("can not set the type for object of class \"", class(object), "\"")
    object
}

.getType <- function(object, fullName = T, collapse = "."){
    if(is(object, "protoCellDefinition")) return(object[["type"]])
    if(is.null(object)) return("NULL")
    if(!is(object, "envProtoClass"))
        stop("The 'object' argument must be of class 'protoCellDefinition' or 'envProtoClass',  suplied an object of class ",  class(object))
    if(fullName){
        type_local <- function(x, type_chain){
            x <- as.environment(x)
            if(isRoot(x)) c(type_chain, x[[".type"]])
            else Recall(x[[".prototype"]], c(type_chain, x[[".type"]]))
        }
        type <- type_local(object, c())
        if(is.character(collapse))
            paste(type, collapse = collapse)
        else
            type
    }else{
        as.environment(object)[[".type"]]
    }
}


###_* GRAPH

leafNames <- function(cellContainer){
    "return leaf cells names from the container"
    cont_env <- as.environment(cellContainer)
    allC <- unlist(eapply(cont_env, function(el)
                          format(as.environment(el))))
    protC <- unlist(eapply(cont_env, function(el)
                           if(is.null(out <- get(".prototype", envir = el)))
                           format(out)
                           else
                           format(as.environment(out))))
    names(allC)[!allC %in% c(protC, "NULL")]
}

## .as_cellContainer_graphNEL <- function(from){
##     require(graph)
##     env_from <- as.environment(from)
##     all_names <- ls(env_from, all.names = TRUE)
##     leaf_names <- leafNames(from)
##     .gr <- new("graphNEL", nodes=all_names, edgemode = "directed")
##     ## NODES
##     nodeDataDefaults(.gr, "short_name") <- "?"
##     nodeDataDefaults(.gr, "type") <- "?" # "leaf" or "prototype"
##     nodeData(.gr, all_names, "short_name") <- sapply(all_names, function(nm) get("type", envir = env_from[[nm]]))
##     nodeData(.gr, all_names, "type") <- "prototype"
##     nodeData(.gr, leaf_names, "type") <- "leaf"
##     ## EDGES
##     edgeDataDefaults(.gr, "type") <- "?" # "model" or "prototype"
##     ## add edges: based only on leaf_names!
##     lapply(leaf_names, function(nm){
##         rec_names <- unlist(strsplit(nm, split = ".", fixed = TRUE))     ## "aa" "bb" "cc"
##         cum_names <-
##             Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
##                    c(paste(y, x[[1]], sep = "."), x), rev(rec_names), right = FALSE)
##         suppressWarnings({
##             .gr <<- addEdge(cum_names[ -1], cum_names[-length(cum_names)], .gr)
##             edgeData(.gr, cum_names[ -1], cum_names[-length(cum_names)], "type") <<- "prototype"
##         })
##     })
##     .gr
## }


## ## gR <- as(M[[".cells"]], "graphNEL")
## ## str(renderGraph(layoutGraph(gR)))
## ## plot(gR)
## ## edges(gR)

## plotCellGraph <-
##     function(x, y, col.prototype="cyan",
##              col.leafs="yellow",
##              layoutType = c("dot","neato","twopi","circo","fdp"),
##              plot.root=F, container = ".cells",
##              fill = "Set2", #if character of length 1, it is a brewer palette name
##              shape = c("circle", "ellipse", "box"),
##              col = c("black", "darkgreen", "darkblue"),
##              cex = 1,
##              lwd = 2,
##              textCol = "black",
##              bg = "gray20",
##              ...){
##         x <- as(x, "graphNEL")
##         old_par <- par(bg="gray20")
##         if(!plot.root){
##             x <- subGraph(nodes(x)[!nodes(x)%in%"*"], x)
##         }
##         localTypeProps <- function(prop, nr, type, names)
##             setNames(rep(prop, length.out = nr)[type], names)
##         ## rearange common labels such that same type nodes and edges have same graphical properties:
##         typeNodes <- unlist(sapply(nodeData(x), function(nd) nd$type))
##         typeEdges <- unlist(sapply(edgeData(x), function(nd) nd$type))
##         common <- intersect(typeNodes, typeEdges)
##         all_labels <- unique(c(typeEdges, typeNodes))
##         all_labels <- c(common, setdiff(all_labels, common))
##         typeNodes <- factor(typeNodes, levels = all_labels)
##         typeEdges <- factor(typeEdges, levels = all_labels)
##         ## NODES
##         type <- typeNodes
##         labels <- sapply(nodeData(x), function(nd) nd$short_name)
##         nodeNames <- names(type)
##         if(is.character(fill) && length(fill) == 1L){
##             library(RColorBrewer)
##             fill <- brewer.pal(max(length(levels(type)), 3L), fill)
##         }
##         nrt <- length(levels(type))
##         nodeRenderDefinition(x) <-
##             list(lwd = localTypeProps(lwd, nrt, type, nodeNames),
##                  cex = localTypeProps(cex, nrt, type, nodeNames),
##                  textCol = localTypeProps(textCol, nrt, type, nodeNames),
##                  col = localTypeProps(col, nrt, type, nodeNames),
##                  fill = localTypeProps(fill, nrt, type, nodeNames),
##                  shape = localTypeProps(shape, nrt, type, nodeNames),
##                  label = labels)
##         ## EDGES
##         type <- typeEdges
##         edgeNames <- names(type)
##         names(type) <- edgeNames <- gsub("\\|", "~", edgeNames)
##         ## if(is.character(fill) && length(fill) == 1L)
##         ##     fill <- brewer(max(length(levels(type)), 3L), fill)
##         edgeRenderDefinition(x) <-
##             list(col = localTypeProps(fill, nrt, type, edgeNames))
##         ## LAYOUT
##         layoutType <- layoutType[[1]]
##         x <- layoutGraph(x, layoutType=layoutType)
##         renderGraph(x)
##         par(old_par)
##     }


###_* INFO

.infoContainer <- function(names, pObject, container){
    tcollate <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", tcollate))
    Sys.setlocale("LC_COLLATE", "C")
    containerEnv <-
        if(is.environment(container))
            container
        else
            as.environment(get(container, envir = pObject))
    all_names <- c()
    out <- data.frame(row.names = names)
    while(!(identical(containerEnv, emptyenv()) || is.null(pObject))){
        names_in <- names %in% ls(containerEnv, all.names = TRUE)
        type <- get(".type", envir = pObject)
        out <- cbind(names_in, out)
        names(out)[[1]] <- type
        containerEnv <- parent.env(containerEnv)
        pObject <- get(".prototype", envir = pObject)
    }
    iout <- as.matrix(out)
    out[iout] <- " +"
    out[!iout] <- "  "
    out <- out[order(row.names(out)),, drop = FALSE]
    predots <- unlist(lapply(strsplit(row.names(out), ".", fixed = T),
                             function(el) paste(rep(". ", length(el) - 1), collapse = "")))
    row.names(out) <- paste(predots, row.names(out), sep = "")
    out
}

.infoCell <- function(object){
    if(!is(object, "envProtoClass")) stop("Not a valid object from class envProtoClass")
    info_local <- function(x, info_chain){
        x <- as.environment(x)
        info <- c(cellType = x[[".type"]],
                  homeCxtType = .getType(x[[".self"]][[".homeContext"]]),
                  class = class(x[[".self"]]),
                  homeCxtClass = class(x[[".self"]][[".homeContext"]]))
        if(isRoot(x)) rbind(info, info_chain)
        else Recall(x[[".prototype"]], rbind(info, info_chain))
    }
    out <- info_local(object, data.frame(cellType = "--",
                                         homeCxtType = "---",
                                         class = "--",
                                         homeCxtClass = "--"))
    out[-nrow(out), ]
}

.infoContext <- function(object){
    if(!is(object, "protoContext")) stop("Not a valid object from class protoContext")
    info_local <- function(x, info_chain){
        x <- as.environment(x)
        info <- c(contextType = x[[".type"]],
                  class = class(x[[".self"]]))
        if(isRoot(x)) rbind(info, info_chain)
        else Recall(x[[".prototype"]], rbind(info, info_chain))
    }
    out <- info_local(object, data.frame(contextType = "--",
                                         class = "--"))
    out[-nrow(out), ]
}


infoForms <- function(pObject, names = NULL, regexp = NULL, all.names = FALSE,
                       expandCode = FALSE) {
    ## breaks down the changes tot he forms in the proto - history
    names <-
        if(!is.null(regexp)){
            ## all.names ignrored
            names <- .get_all_names(get(".forms", envir = pObject))
            grep(regexp, names, value = TRUE)
        }else if(all.names){
            ## names ignored
            .get_all_names(get(".forms", envir = pObject))
        }else if(is.null(names)){
            names <- .get_all_names(get(".forms", envir = pObject))
            names[names %in% ls(pObject, all.names =  TRUE)]
        }
    res <- replicate(length(names),
                     data.frame(check.names = FALSE), simplify = FALSE)
    names(res) <- names
    while(!is.null(pObject)){
        type <- get(".type", envir = pObject)
        proto <- get(".prototype", envir = pObject)
        for(nm in names){
                                        #            eval(substitute(
            ## blanks if not "nm" is not defined in pObject
            res[[nm]][[type]] <- rep(list(" "), NROW(res[[nm]]))
            if(exists(nm, envir = pObject, inherits = FALSE)){
                obj <- get(nm, envir = pObject, inherits = FALSE)
                if(!is(obj, "protoForm"))
                    warning("Object \"", nm, "\" in the pObject \"", type, "\", is not a protoForm.")
                names_obj <- names(obj)
                ## compare with the inherited object
                if(is.null(proto) ||
                   !exists(nm, envir = get(".forms", envir = proto))){
                    proto_obj <- list()
                }else{
                    proto_obj <- get(nm, envir = proto)
                }
                ## Names in obj:
                for(nm_el in names_obj){ # as.character needed here, if sapply returns list()
                    res[[nm]][[nm_el, type]] <-
                        if(length(obj) == 0)
                            structure("0", code = deparse(obj[[nm_el]]))
                        else if(is.null(proto_obj[[nm_el]]))
                            ## added
                            structure("+", code = deparse(obj[[nm_el]]))
                        else if(identical(proto_obj[[nm_el]], obj[[nm_el]]))
                            ## same
                            structure("=", code = deparse(obj[[nm_el]]))
                        else
                            ## different
                            structure("/", code = deparse(obj[[nm_el]]))
                }
                ## Names deleted:
                if(length(removed_names <-
                          names(proto_obj)[!names(proto_obj) %in% names_obj]))
                    res[[nm]][removed_names, type] <- rep.int("-", length(removed_names))
                ## doc and code for nm in type object
                attr(res[[nm]][[type]], "doc") <- obj@doc
                attr(res[[nm]][[type]], "code") <- deparse(obj)
            }
        }
        pObject <- proto
    }
    res <- mapply(function(el, nm){
        el[is.na(el)] <- " "
        if(nrow(el) == 0L){
            el[1, ] <- "0"## fixme:  expression() will give an 0 - row df here
        }
        el[" "] <- "|"
        el$form <- paste(formatC(seq.int(NROW(el)), digits = 2), row.names(el))
        el <- rbind(rep.int(" ", length(names(el))), el)
        el$form[[1]] <- paste("", nm, collapse = "")
        row.names(el) <- NULL
        el[rev(names(el))]
    }, res, names(res), SIMPLIFY = FALSE)
    class(res) <- c("infoForms", class(res))
    res
}

as.data.frame.infoForms <- function(x){
    for(nm in names(x)){
        x[[nm]]$`  ` <- "|"
        x[[nm]]$`  `[[1]] <- " "
        x[[nm]]$code <-
            apply(x[[nm]], 1, function(row){
                code <- NULL
                col_nr <- length(row)
                while(is.null(code) && col_nr > 2){
                    code <- attr(row[[col_nr]], "code")
                    col_nr <- col_nr - 1L
                }
                code <- paste(code, collapse = " ")
                if(nchar(code) < 30)
                    code
                else if (length(grep("^e\\(", code)))
                    "e(...)"
                else
                    "< Expression >"
            })
        ## if(is.na(res[[nm]][nm_el, "<code>"])){
    }
    res <- do.call(rbind, unname(x))
    res$form <- format(res$form, justify = "left")
    res
}

print.infoForms <- function(x, ...){
    print.data.frame(as.data.frame.infoForms(x))
}

## templateFile <- "template.html" ## system.file("brew", "default", "results.brew.html",
## ##             package = "sos")
## template <- file(templateFile, encoding = "utf-8", open = "r")
## xenv <- new.env()
## assign("form", "my form", envir = xenv)
## assign("x", x, envir = xenv)
## assign("title", "my title", envir = xenv)
## brew(template, File, envir = xenv)
## close(template)
## browseURL(File)

## browseDefinition <-
##     function (x, File, title = "Definition Browser", header1 = "", header2 = "",
##               openBrowser = TRUE, stylesheet = "info.dark", ...)
## {
##     ## x should be a df.
##     library(brew)
##     library(highlight)
##     if (nrow(x) < 1) {
##         cat("x has zero rows;  nothing to display.\n")
##         if (missing(where))
##             where <- ""
##         return(invisible(where))
##     }
##     if (missing(File))
##         f0 <- tempfile()
##     for (i in 1:111) {
##         File <- paste(f0, ".html", sep = "")
##         fInf <- file.info(File)
##         if (all(is.na(fInf)))
##             break
##         f0 <- paste(f0, "1", sep = "")
##     }
##     Dir <- dirname(File)
##     if (Dir == ".") {
##         Dir <- getwd()
##         File <- file.path(Dir, File)
##     }else{
##         dc0 <- dir.create(Dir, FALSE, TRUE)
##     }
##     js <- file_path_as_absolute("~/works/R_dev/pbm/info.js")
##     css <-
##         switch(stylesheet,
##                info.dark = file_path_as_absolute("~/works/R_dev/pbm/info.dark.css"),
##                file_path_as_absolute("~/works/R_dev/pbm/info.css")
##                )
##     if (!file.exists(js)) {
##         stop("Unable to locate '", js, "' file")
##     }else if(!file.exists(css)){
##         stop("Unable to locate '", css, "' file")
##     }else {
##         file.copy(js, Dir, overwrite = TRUE)
##         file.copy(css, Dir, overwrite = TRUE)
##     }
##     templateFile <- "~/works/R_dev/pbm/template.html"
##     if (!file.exists(templateFile))
##         stop("Unable to locate '~/works/R_dev/pbm/template.html' file")
##     template <- file(templateFile, encoding = "utf-8", open = "r")
##     xenv <- new.env()
##     assign("header1", header1, envir = xenv)
##     assign("header2", header2, envir = xenv)
##     assign("x", x, envir = xenv)
##     assign("title", title, envir = xenv)
##     assign("stylesheet", basename(css))
##     brew(template, File, envir = xenv)
##     close(template)
##     FileDefinition <- file.info(File)
##     if (is.na(FileDefinition$size) || FileDefinition$size <= 0) {
##         if (is.na(FileDefinition$size)) {
##             stop("Brew did not create file ", File)
##         }
##         else {
##             stop("Brew created a file of size 0")
##         }
##     }else if (openBrowser) {
##         if (is.na(FileDefinition$size)) {
##             warning("Did not create file ", File, ";  nothing to give to a browser.")
##         }
##         else {
##             if (FileDefinition$size <= 0) {
##                 warning("0 bytes in file ", File, ";  nothing to give to a browser.")
##             }
##             else {
##                 browseURL(File)
##             }
##         }
##     }
##     invisible(File)
## }

## browseForms <-
##     function(pObject, names = NULL, regexp = NULL, all.names = FALSE, stylesheet = "info.dark", ...) {
##         x <- as.data.frame(.infoForms(pObject = pObject, names = names, regexp = regexp,
##                                       all.names = all.names))
##         title <- gettextf("Form info for %s of type \" %s\"",
##                           class(pObject), .getType(pObject))
##         header1 <- title
##         header2 <- paste("Definition for the following names: \n",
##                          paste(names, collapse = ", "))
##         browseDefinition(x, title = title, header1 = header1, header2 = header2, stylesheet = source, ...)
##     }

source("~/works/protoClasses/R/classes.R")

## Local Variables:
## ess-roxy-template-alist: (
##  ("description" . "..description")
##  ("details" . "..details")
##  ("title" . "")
##  ;;("rdname" . "")
##  ("param" . "")
##  ;;("return" . "Object of class \\code{xxx} containing slot ")
##  ("author" . "Vitalie Spinu (\\email{spinuvit@@gmail.com})")
##  ("export" . "")
##  ("seealso" . "\\code{\\link{protoClasses-package}} \\code{\\link{protoContext}}")
##  ;;("references" . "\\url{https://docs.developer.betfair.com/betfair/}")
##  )
## end:


te <- expression({
    c <- a + b
    foo({a + c
         ppp})
    34 <- b + c
})
