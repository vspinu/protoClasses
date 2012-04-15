
options(protoClasses = list(changeCallEnv = FALSE))
## if TRUE  eval(substitute(list(...)) gets rid of source code !!!
## fixme: !!

## Classes to support POP-style classes with reference-semantics for fields
## object-based methods, protoCells and qexps..
## Implementation of the R-based version of these classes (using environments)

## The "envProtoClass" is the basic class from which protoContext and protoCell
## are inhereted.

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

setContextClass <- function(Class,
                            defaultContext = new("protoContext", type = paste("@", Class, sep = "")), ## not root by default
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
##' @title
##' @param envProtoObj
##' @author Vitalie Spinu
##' @examples
isRoot <- function(envProtoObj)
    exists(.rootMetaName, envir = as.environment(envProtoObj), inherits = FALSE)

.signAsRoot <- function(envProtoObj)
    assign(.rootMetaName, TRUE, envir = as.environment(envProtoObj), inherits = FALSE)

##' TRUE is it's a default context
##'
##' @title
##' @param envProtoObj
##' @author Vitalie Spinu
##' @examples
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
    e = eval(substitute(function(expr) .Internal(eval(expr, envir, envir)), list(envir = objEnv)))
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

###_ envProtoClass
.initializeRoot_envProtoClass <- function(.Object,  ##TODO: !! get the "pure" initialization functionality into separate slot "initialize" in class definition!!
                                          initForms = list(),
                                          initFields = list(),
                                          initMethods = list(),
                                          type = "*",
                                          ...){ #... not used here pu
    ## initialize the basic functionality for the ROOT object
    ## .fields, .prototype, basic methods etc
    objEnv <- as.environment(.Object) ## todo: should be base-namespace??
    .signAsRoot(objEnv)
    parent.env(objEnv) <- globalenv()  ## if cell,   will have non empty parent
    ## tothink: lock these fields?
    objEnv[[".fields"]] <- new("protoContainer", typeContainer = ".fields") ## emptyenv as parent by default
    objEnv[[".methods"]] <- new("protoContainer", typeContainer = ".methods")
    objEnv[[".forms"]] <- new("protoContainer", typeContainer = ".forms")
    .initFields(list(type = type), where = objEnv)
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
                   setFields = function(..., changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else list(...)
                       .generic_setter(dots, .self, ".fields")
                   },
                   initForms = function(..., after = NULL, changeCallEnv = getOption("protoClasses")$changeCallEnv) {
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else  list(...)
                       .initForms(dots, .self, after = after)
                   },
                   setForms = function(..., changeCallEnv = getOption("protoClasses")$changeCallEnv){
                       dots <-
                           if(changeCallEnv) eval(substitute(list(...)), envir = .self)
                           else list(...)
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

    ## SPECIALS
    .insertSpecial(objEnv, self = .Object, prototype=NULL)
    objEnv[[".root"]] <- .Object

    ## "USER" FIELDS:
    .initFields(initFields, .Object)
    .initMethods(initMethods, .Object)
    .initForms(initForms, .Object)
    .Object
}

## .dummy_envProtoObject <- newRoot("envProtoClass")
.initialize_envProtoClass <- function(.Object,
                                      prototype = newRoot("envProtoClass"),
                                      type = "--",
                                      initMethods = list(), initFields = list(), initForms = list(),
                                      methods = list(), fields = list(), forms = list(),
                                      expr = expression(),
                                      changeCallEnv = getOption("protoClasses")$changeCallEnv,
                                      ...){
    .Object <- callNextMethod()

    ## !!!!! NO CLONING,  ALWAYS A NEW OBJECT !!!!!!!! ##
    objEnv <- .Object@.xData <- new.env(TRUE)

    ## BASIC VALIDATION:
    if(!is(prototype, "envProtoClass")) # tothink: prototype should be from the same class as .Object??
        stop("Class of prototype argument must extend \"envProtoClass\".\n Got an object of class \"", class(prototype), "\"")
    isValidProtoObject(prototype,trigger_error = TRUE)
    parent.env(objEnv) <-
        protoEnv <- as.environment(prototype)

    ## FUNDAMENTAL CONTAINERS:
    objEnv[[".fields"]] <- new("protoContainer", parentContainer = protoEnv[[".fields"]], typeContainer = ".fields")
    objEnv[[".methods"]] <-new("protoContainer", parentContainer = protoEnv[[".methods"]], typeContainer = ".methods")
    objEnv[[".forms"]] <- new("protoContainer",  parentContainer = protoEnv[[".forms"]], typeContainer = ".forms")

    ## SPECIALS
    .insertSpecial(objEnv, self= .Object, prototype = prototype)
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
    if(length(fields))
        .generic_setter(fields, .Object, ".fields")
    if(length(methods))
        .generic_setter(methods, .Object, ".methods")
    if(length(forms))
        .generic_setter(forms, .Object, ".forms")
    eval(expr, envir = objEnv)
    .Object
}

###_ + protoContext
.initializeRoot_protoContext <- function(.Object, rootParentEnv = globalenv(), ##fixme:  make field:
                                         initCells = list(), ...){
    .Object <- callNextMethod(.Object, ...) ##envProtoClass
    objEnv <- as.environment(.Object)
    objEnv[[".cells"]] <-  new("cellContainer", typeContainer = ".cells") ## empty parent by default
    objEnv[[".rootParentEnv"]] <- rootParentEnv
    .initMethods(list(initCells =
                      function(...){
                          dotsq <- substitute(list(...))
                          dots <- list(...)
                          for(i in seq_along(dots)){
                              ## infer the "type" of the cell from the names if type == "--"
                              if(is.name(dotsq[[i + 1L]]) && is(dots[[i]], "protoCell")&& .type(dots[[i]]) == "--")
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
    objEnv[[".cells"]] <-  new("cellContainer",
                               parentContainer = parent.cells,
                               typeContainer = ".cells")
    objEnv[[".self"]] <- .Object
    objEnv[[".rootParentEnv"]] <-
        if(is.null(rootParentEnv))
            get(".rootParentEnv", envir = objEnv[[".prototype"]])
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
                                  homeContext = NULL,
                                  type = "--",
                                  initFields = list(), initMethods = list(), initForms = list(),
                                  fields = list(), methods = list(), forms = list(),
                                  expr = expression(),
                                  ...){
    ## PROTOTYPE can be a character string or a valid envProtoObject
    ## ---
    ## Clone the CELL if CELL of type_long exists in the inherited container,
    ## create a new one otherwise.
    ## CELL is NOT installed in the context.
    ## note: cannot create the root cell!! supplying * as TYPE produces the cell *.*
    ## CONTEXT
    if(is.null(context <- homeContext))
                                        #default context
        context <- getClassDef(getClassDef(class(.Object))@contextClass)@defaultContext
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

    ## CREATE OR CLONE THE CELL
    type <- as.character(type)
    type_long <- paste(type, .type(prototype), sep = ".")
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

    if(!exists(type_long, envir = parent.env(.cells))){
        ## create a NEW one:
        .Object <- callNextMethod(.Object, prototype = prototype, type = type, ...)
    }else{
        ## type_long is FOUND so  CLONE:
        the_cell <- get(type_long, envi = parent.env(.cells))
        obj <- as.environment(clone(the_cell))
        ## REDIRECT parents of containers   ## note: no problems with cloning * cell; it's just not allowed to create *
        parent.env(get(".forms", envir = obj)) <- get(".forms", envir = prototype)
        parent.env(get(".methods", envir = obj)) <- get(".methods", envir = prototype)
        parent.env(get(".fields", envir = obj)) <- get(".fields", envir = prototype)
        .Object@.xData <- as.environment(obj) ##!! do not replace the incoming object class!!!
    }


    ## Special OBJECTS:
    objEnv <- as.environment(.Object)
    objEnv[[".homeContext"]] <- homeContext
    objEnv[[".self"]] <- .Object

    ## User INITS:
    .initFields(initFields, .Object)
    .initMethods(initMethods, .Object)
    .initForms(initForms, .Object)
    if(length(fields))
        .generic_setter(fields, .Object, ".fields")
    if(length(methods))
        .generic_setter(methods, .Object, ".methods")
    if(length(forms))
        .generic_setter(forms, .Object, ".forms")
    eval(expr, envir = objEnv)
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
    ## force  evaluation  of encapsulated expression F.(...) or forms directly form()a
    pframe <- parent.frame()
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
    checks <-
        c(`Object is not of class "envProtoClass"` = is(object, "envProtoClass"),
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
    if(is.recursive(obj))
        identical(obj[[1L]], as.name("e"))
    else
        FALSE
}

.assignFormBasic <- function(formName, form, where, register = TRUE, after = NULL){
    "Assign the form if not present, or insert after AFTER to the existing one in WHERE.\n'
If AFTER is NULL or >= length of existing form - append. if <= 0 - preppend.
No name checks.\n
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
            if(is.null(after) || after >= length(oldForm)){
                newForm <- oldForm
                for(nm in allNames(form))
                    newForm[[nm]] <- form[[nm]] ## form should replace the old one
                newForm
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
        ## default install binding in the .forms container
        installBinding(new("formInfo", formClass = class(newForm)),
                       where, formName, ".forms")
    }
    do_assign
}


.installBinding_protoForm <- function(bindInfo, where, bindName, container = ".forms",
                                      after = NULL){
    ## Used recursively:
    ## if bindInfo is form - assign in the whereEnv
    ## if bindInfo is an expression - just return it
    ## If the form with name of the form aa.bb.cc is assigned,
    ## check for existence of aa,  aa.bb, aa.bb.cc and install in the current
    ## container if modified (ie if not identical() to what is already installed
    ## in the container)
    ## If bindInfo == NULL then all the forms (inlusive this) are removed from where
    ## if AFTER is nonnull it must be character or an integer specifying
    ## the position new forms to be inserted.
    form <- bindInfo
    whereEnv <- as.environment(where)
    rec_names <- unlist(strsplit(bindName, split = ".", fixed = TRUE))     ## "aa" "bb" "cc"
    cum_names <- Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
                        c(paste(x[[1]], y, sep = "."), x), rec_names)
    thisName <- cum_names[[1]] ## aa.bb.cc
    do.assign <- TRUE
    if(is.null(form))
        ##NULL
        return(.removeFormWithChildren(bindName, whereEnv))
    else if(length(form) == 0){
        form <- .F()
        if(!exists("bindName", envir = whereEnv[[".forms"]])){
            assign(bindName, form, envir = whereEnv)
            ## default install binding in the .forms container
            installBinding(new("formInfo", formClass = class(form)),
                           whereEnv, bindName, ".forms")
        }
        ##.emptyFormWithChildren(bindName, whereEnv) ##.F()
    }
    if(is(form, "protoForm")){
        ## create (or append to) parent formss (i.e. aa.bb, aa)
        while(do.assign)
            if(length(cum_names) > 1){
                do.assign <-
                    ## aa.bb = form(cc = e(aa.bb.cc))
                    .assignFormBasic(cum_names[[2]],
                                     .makeEForm(cum_names[[1]]), whereEnv, after = after)
                cum_names <- cum_names[-1]
            }else{
                do.assign <- FALSE
            }
        ## shortNames (dd, ff)
        ## Do not allow "." in short names (fixme: split them?)
        shortNames <- names(form)
        if(length(dot_here <- grep(".", shortNames, fixed = TRUE))){
            warning("\".\" was replaced with \"_\" in ",
                    paste(shortNames[dot_here], collapse=", "))
            shortNames <- gsub(".", "_", shortNames, fixed = TRUE)
        }
        ## longNames (aa.bb.cc.dd, aa.bb.cc.ff)
        longNames <- paste(thisName, shortNames, sep = ".")
        ## Get the existing aa.bb.cc form:
        newForm <-
            oldForm <-
                if(exists(thisName, envir = get(".forms", envir = whereEnv)))
                    get(thisName, envir = whereEnv)
                else new("protoForm")
        ## install each element of the "form" (a form or an expression)
        for(i in seq_along(shortNames)){
            nm <- shortNames[[i]]
            newForm[[nm]] <-
                .installBinding_protoForm(form[[i]], whereEnv, longNames[[i]]) #can not use generic here, form[[i]] can be an expressiong
        }
        ## install newForm only if different
        if(!identical(newForm, oldForm)){
            ## DON'T REMOVE ANY MISSING FORMS FROM THE PROTOOBJECT !! TOTHINK:
            ## Against removing:
            ## a) exec forms and coceptual containers are separeted
            ## b) recursive removing complicates the design
            ## c) Not clear if to remove sub - forms only in current cell or sub - types as well
            ## Pro removing: a) protoForms behave like genuine recursive objects in R
            assign(bindName, newForm, envir = whereEnv)
            ## default install binding in the .forms container
            installBinding(new("formInfo", formClass = class(form)),
                           whereEnv, bindName, ".forms")
        }
        return(.makeEexpr(thisName))
    }else{
        ## .assignForm should no be used directly to assign non protoForm objects !!
        return(form)  ## returns as is (i.e. expression), nothing is assigned
    }
}

.initForms <- function(forms, where, after = NULL){
    "init the FORMS in the object WHERE"
    if(length(forms) == 1L && is.list(forms[[1L]]))
        forms <- forms[[1L]]
    if(is.character(forms)) {
        ## treat as empty "expressions"
        formNames <- forms
        forms <- as.list(rep(expression(), length(forms)))
        names(forms) <- formNames
    }
    else if(is.list(forms)) {
        ## non empty names
        formNames <- names(forms)
        if(length(forms)>0 && ( is.null(formNames) ||
                               !all(nzchar(formNames))))
            stop("A list argument for forms() must have nonempty names for all the suplied object")
        if(!(all(which <- unlist(lapply(forms, function(x) is.language(x) || is.null(x))))))
            stop("Arguments to forms() must be a class extending name, call or expression,
or a list of these /see ?is.language/. Not true for ", paste(formNames[!which], collapse = ", "))
    }
    else
        stop(gettextf("Improper argument for forms(), must be a list or a character vector ; got an object of class \"%s\"",
                      class(forms)), domain = NA)
    whereEnv <- as.environment(where)
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
        installBinding(forms[[i]], where, formNames[[i]], ".forms", after = after)
    return(invisible(formNames))
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
    out <-
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
    out
}


###_  * GETTERS
.existsMethod <- function(name, selfEnv){
    exists(name, envir = selfEnv[[".methods"]])
}

.getMethod <- function(name, selfEnv)
    if(exists(name, envir = selfEnv[[".methods"]])){
        meth <- get(name, envir = selfEnv[[".methods"]])
        environment(meth) <- selfEnv
        if(meth@changeCallEnv) meth <- selfEnv[[".PROTOZIZE"]](meth)
        meth
    }else{
        substitute()
    }

.existsField <- function(name, selfEnv){
    exists(name, envir = selfEnv[[".fields"]])
}
.getField <- function(name, selfEnv)
    if(exists(name, envir = selfEnv[[".fields"]])){
        field_fun <- get(name, envir = selfEnv[[".fields"]])
        environment(field_fun) <- selfEnv
        field_fun()
    }else{
        substitute()
    }

.getFieldFunction <- function(name, selfEnv){
    "Gets the function execute when the fields is requested"
    if(exists(name, envir = selfEnv[[".fields"]])){
        field_fun <- get(name, envir = selfEnv[[".fields"]])
        environment(field_fun) <- selfEnv
        field_fun
    }else{
        NULL
    }
}

.existsForm <- function(name, selfEnv){
    exists(name, envir = selfEnv[[".forms"]])
}

.getForm <- function(name, selfEnv)
    if(exists(name, envir = selfEnv[[".forms"]])){
        new("protoFormWithEnv", get(name, envir = selfEnv), environment = selfEnv)
    }else{
        substitute()
    }


.existsCell <- function(name, selfEnv){
    exists(name, envir = selfEnv[[".cells"]])
}


## .existsPartialCell <- function(name, selfEnv){
##     .existsPartial(name, selfEnv[[".cells"]])
## }

.getCell <- function(name, selfEnv){
    .cells <- as.environment(selfEnv[[".cells"]])
    .getPartial(name, .cells, trigger_error = FALSE)
}

###_  * SETTERS
## SET new value only if the object already exists in the hierarchy
.setField <- function(x, name, value){ # must follow dollar arguments, dont realy like this :(
    .fields <- get(".fields", envir = x)
    if(exists(name, envir = .fields)){
        field_fun <- get(name, envir = .fields)
        environment(field_fun) <- as.environment(x)
        field_fun(value) ## side effect here (must assign to environment)
        return(x)
    }else{
        stop("Object \"", name, "\" is not a valid field in the protoObject of type \"", .type(x), "\"")
    }
}

.setMethod <- function(x, name, value){
    .methods <- get(".methods", envir = x)
    if(exists(name, envir = .methods)){
        ## oldMeth <- get(name, envir = x) ;; need to compare with an old one really?  probably not
        if(!is.function(value))
            stop("Methods must be functions. Not true for '", name, "'")
        method <- new("protoMethod", value)
        installBinding(method, x, name, ".methods")
    }else{
        stop("Object \"", name, "\" is not a valid method in the protoObject of type \"", .type(x), "\"")
    }
}

.setForm <- function(x, name, value, after = NULL){
    ## after must be a character or position
    .forms <- get(".forms", envir = x)
    if(exists(name, envir = .forms)){
        oldForm <- get(name, envir = x)
        is_eCall <- unlist(sapply(oldForm, isECall))
        shortNames <- names(oldForm)
        longNames <- paste(name, shortNames, sep = ".")
        lapply(seq_along(names(oldForm)[is_eCall]), function(nm){
            ## shiit!! FIXME:: TODO: does not work for complex forms
            ## if use '!is(value[[nm]], "protoForm") && ' in check the protoForm
            ## goes to instalation and installes subforms even if they are not present yet!!
            if(!identical(oldForm[[nm]], value[[nm]]))
                stop("Can only modify non-form elements with \"forms\" interface. \n Not true for element \"", longNames[[nm]], "\"\n Use \"initForms\" to install/modify forms.")
        })
        value <- as(value, "protoForm")
        if(!identical(oldForm@.Data, value@.Data))
            installBinding(value, x, name, ".forms")
    }else{
        stop("Object ", name, " is not a valid form in the protoObject of type \"", get("type", x), "\"")
    }
}
.setCell <- function(x, name, value){}

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
                stop("Object ", name, " is not a valid method, field or form in the protoObject of type ", .type(x))
        }
    }
    out
}

.dollarSet_envProtoClass <- .setField

.dollarGet_protoContext <- function(x, name){
    if(!is.null(obj <- .getCell(name, as.environment(x))))
        return(obj)
    else
        .dollarGet_envProtoClass(x, name) ## todo: error message is misleading
}

.dollarSet_protoContext <- function(x, name, value){ # must follow dollar arguments, dont realy like this :(
    .fields <- get(".fields", envir = x)
    .cells <- get(".cells", envir = x)
    match <- .complete_partial_name(name, .cells)
    if(!(is.na(match)||match == 0L)){
        if(!is(value, "protoCell"))
            stop(gettextf("cannot assign an object of class \"%s\" as a cell \"%s\"",
                          class(value), match))
        assign(match, value, envir = .cells)
    }else if(exists(name, envir = .fields)){
        field_fun <- get(name, envir = .fields)
        environment(field_fun) <- as.environment(x)
        field_fun(value) ## side effect here (must assign to environment)
    }else{
        stop("Object \"", name, "\" is not a valid cell or field  in the protoContext of type \"", .type(x), "\"")
    }
    invisible(x)
}




###_ + FIELDS
.initialize_Field <- function(.Object,  ...){
    .Object <- callNextMethod()
    name <- .Object@bindName
    className <- .Object@className
    if(is.null(body(.Object))){
        ## default fields (i.e. function was not supplied)
        metaName <- as.character(name)
        if(length(className) == 0L || identical(className, "ANY")){
            ## "ANY"
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
            className <- "ANY"
        }else if(isVirtualClass(className)){
            ## "VIRTUAL"
            .Object@.Data  <-
                eval(substitute(function(value){
                    if(missing(value)){
                        dummyField
                    }else{
                        if(is(value, dummyClass)){
                            value <- as(value, dummyClass)
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
    .Object@className <- if(length(className) == 0L)  "custom" else className
    .Object
}

.initFields <- function(fields, where, classes = list()){
    "Install the FIELDS in the object WHERE"
    ## can supply NULL to fields or "NULL" class to remove
    fieldClasses <- character()
    fieldInits <- list()
    fieldNames <- character()
    if(length(classes) > 0L){
        fields <- as.list(classes)
        ## classes or NULL
        fieldNames <- names(fields)
        if(length(fields)> 0 && (is.null(fieldNames) || !all(nzchar(fieldNames))))
            stop("Classes argument to initFields must have nonempty names")
        if(!all(sapply(fields, is.character)))
            stop("Classes argument to initFields must be a character vector or list of strings.")
        fieldClasses <- as.character(fields) ## converts NULL to "NULL":)
        fieldInits <- sapply(fieldClasses, new)
    }
    ## TREAT AS A LIST OF INITIAL VALUES
    fieldNames1 <- names(fields)
    if( length(fields)> 0 && (is.null(fieldNames1) ||
                              !all(nzchar(fieldNames1))))
        stop("Arguments to initFields must have nonempty names")
    fieldClasses <- c(fieldClasses, lapply(fields,  class))
    fieldInits <- c(fieldInits, fields)
    fieldNames <- c(fieldNames, fieldNames1)

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
            if(fieldClasses[[i]] == "protoField"){
                ## protoField is suplied, thus don't assign initial value in WHERE
                new("protoField", fieldInits[[i]], bindName = fieldNames[[i]])
            }else{
                ## the default fields; assign initial value in WHERE
                assign(fieldNames[[i]], fieldInits[[i]], envir = whereEnv)
                new("protoField",
                    bindName = fieldNames[[i]],
                    className = fieldClasses[[i]])
            }
        installBinding(field, whereEnv, fieldNames[[i]], ".fields")
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
        installBinding(method, whereEnv, methodNames[[i]], ".methods")
    }
    return(methodNames)
}

###_ + CELLS
.initCells <- function(cells, where){
    "Install the CELLS in the object WHERE"
    ## cells must be a list,  names(cells) have precedence over internal type
    ## if el of cells is 'character' it is looked up in the .cells,  if not
    ## found produce an error!!
    ## do not overwrite names in the program!
    ##
    if(!is(where, "protoContext")) stop("Argument \"where\" must extend the class \"protoContext\"")
    if(is.list(cells)) {
        ## can have empty names, will be taken from type.
        cellTypes <- allNames(cells)
        if(!all(sapply(cells, function(el)
                       is.null(el) ||
                       is(el, "protoCell") ||
                       is(el, "cellInfo") ||
                       is(el, "character")))) ## must be one of existing cells! don't create a new cell!!
            stop("Argument for initCells must be of class  \"protoCell\" , \"cellInfo\" or \"character\" vector of existing types")
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
            cells[[i]] <- .getPartial(cells[[i]], get(".cells", envir = whereEnv), object_name = "cell")
        ##names have precedence over types (types should not be used explicitly at user level)
        installBinding(cells[[i]], where, cellTypes[[i]], ".cells")
    }
    return(invisible(cellTypes))
}

.installCellInContext <- function(cell, context, container = ".cells"){
    ## CELL and PROTOTYPES are cloned if not homeless or not already  associated
    ## with CONTEXT
    ## CELL and missing PROTOTYPES  are inserted into the container, if needed
    ## with the current context. Prototype chain is followed as long as
    ## proto_type is not found in current context.
    .to_clone <- function(cell, context)
        ## CLONE if not homeless or already associated with the context
        !(is.null(homeContext(cell)) ||
          identical(as.environment(context),
                    as.environment(homeContext(cell))))
    stopifnot(is(cell, "protoCell"))
    if(!is(context, tCls <- getClassDef(class(cell))@contextClass))
        stop(gettextf("Suplied context class /%s/ does not extend the default context class of the cell /%s/",
                      class(context), tCls))
    contextEnv <- as.environment(context)
    if(.to_clone(cell, context))
        cell <- clone(cell)
    if(!extends(class(cell), tCls <- getClassDef(class(context))@cellClass))
        cell <- as(cell, tCls)  ## note: some functionality might be missing, provide explicit coerce method.
    cellEnv <- as.environment(cell)
    containerEnv <- as.environment(contextEnv[[container]])
    short_type <- cellEnv[["type"]]
    if(identical(short_type, "--"))
        stop("Cannot install cell of type \"--\"; please supply the type argument.")
    type <- .type(cell)
    cell_to_return <- cell
    prototype <- cellEnv[[".prototype"]]
    ## KEEP INSERTING cell -> prototypes if doesn't exist
    while(!is.null(prototype) &&
          !exists(prot_type <- .type(prototype),
                  envir = containerEnv, inherits = FALSE)){
        ## Disregard the prototype if already exists in the current container!!
        containerEnv[[type]] <- cell
        .redirect_prototypes(cell, containerEnv)
        cellEnv[[".homeContext"]] <- context ## fixme: make a function setHomeContext!
        setPrototype(cell, ## sets parent.env as well
                     if(.to_clone(prototype, context)){
                         clone(prototype)
                     }else{
                         prototype
                     })
        ## ------
        cell <- cellEnv[[".prototype"]]
        cellEnv <- as.environment(cell)
        type <- .type(cell)
        prototype <- cellEnv[[".prototype"]]
    }
    ## NOTE: if cell is already in container nothing changes
    containerEnv[[type]] <- cell
    .redirect_prototypes(cell, containerEnv)
    cellEnv[[".homeContext"]] <- context
    if(is.null(prototype)){ ## root
        parent.env(cellEnv) <- get(".rootParentEnv", contextEnv) ## fixme:  make function getRootParentEnv
    }else{
        setPrototype(cell, containerEnv[[prot_type]])
    }
    invisible(cell_to_return)
}

.redirect_prototypes <- function(prototype, container){
    ##make all the cells which point to .type(prototype) to point to new prototype
    prot_type <- .type(prototype)
    lapply(ls(container, all.names = TRUE), function(nm){
        cell <- get(nm, envir = container, inherits = FALSE)
        if(identical(.type(get(".prototype", envir = cell)), prot_type))
            setPrototype(cell, prototype)
    })
}


cellFromInfo <- function(cellInfo, homeContext){
    stopifnot(is(cellInfo, "cellInfo"))
    ## infer name! SIDE EFFECT (does not work here,  this func is called usually  internally)
    ## nameCell <- substitute(cellInfo)
    ## if(is.name(nameCell) && identical(cellInfo[["type"]], "--"))
    ##     cellInfo[["type"]] <- as.character(nameCell)
    if(is(cellInfo$prototype, "cellInfo"))
        cellInfo[["prototype"]] <- cellFromInfo(cellInfo$prototype, homeContext)
    cellInfo[["homeContext"]] <- homeContext
    do.call("new", c(list(Class = cellInfo@cellClass), cellInfo))
}

homeContext <- function(cell)
    get(".homeContext", envir = cell)

C <- function(...){
    args <- list(...)
    args[["Class"]] <- NULL
    new("cellInfo", args, cellClass = "protoCell")
}

.installBinding_default <- function(bindInfo, where, bindName, container, ...){
    ## default methods just assigns the stuff in the container.
    containerEnv <- as.environment(get(container, envir = where, inherits = FALSE))
    assign(bindName, bindInfo, envir = containerEnv)
}

.installBinding_protoCell <- function(bindInfo, where, bindName, container = ".cells", ...){
    ## bindName has precedence:
    if(!missing(bindName) && !identical(bindName, ""))
        assign("type", bindName, envir = bindInfo)
    else
        bindName <- get("type", envir = bindInfo)
    .installCellInContext(bindInfo, where, container)
}

.installBinding_cellInfo <- function(bindInfo, where, bindName, container = ".cells", ...){
    if(!missing(bindName) && !identical(bindName, ""))
        bindInfo[["type"]] <- bindName
    ## if(!is(where, "protoContext")) stop("Argument \"where\" is not a subclass of \"protoContext\"")
    new_cell <- cellFromInfo(bindInfo, homeContext = where)
    installBinding(new_cell, where, bindName, container = container)
    invisible(new_cell)
}

.installBinding_protoField <- function(bindInfo, where, bindName, container, ...){
    ## assign if different
    containerEnv <- get(container, envir = where)
    if(exists(bindName, envir = containerEnv)){
        oldField <- get(bindName, envir = containerEnv)
        if(!identical(bindInfo, oldField))
            callNextMethod()
        ## else do nothing
    }else{
        callNextMethod()
    }
}

.changeEnvFuncs <- function(envir){
    ## environments of ALL functions from envir is changed to ENVIR
    ## todo: make changeThisOnly argument
    envir <- as.environment(envir)
    all.names <- ls(envir, all = T)
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
    all.names <- ls(envir, all = T)
    lapply(all.names, function(nm)
           if(is(envir[[nm]], "environment" ) && (is.null(changeThisOnly) ||
                                                  identical(parent.env(envir[[nm]]), as.environment(changeThisOnly))))
           parent.env(envir[[nm]]) <- envir)
    invisible(NULL)
}

###_  * CLONES(no side effects)
.clone_envProtoClass <- function(x, exclude_names = c(), ...){
    ## exclude names are not cloned or copied (ex. .cells in protoContext class)
    ## TOTHINK: environments of ALL functions is changed here to point to new environment
    y <- x
    xEnv <- as.environment(x)
    y@.xData <- yEnv <- new.env(TRUE, parent = parent.env(xEnv))
    lapply(xEnv[[".cloneFirst"]], eval, envir = yEnv)
    x_names <- ls(envir=xEnv, all.names=TRUE)
    x_names <- x_names[!(x_names %in% c(".self", ".prototype", as.character(xEnv[[".cloneExclude"]]),
                                        ".homeContext", exclude_names))]
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
        yEnv[[".root"]] <- y
        yEnv[[".prototype"]] <- NULL ## just in case
    }

    .changeEnvFuncs(yEnv) ## fixme: should change environments only of those which point to xEnv!!!
    .changeEnvEnvs(yEnv, changeThisOnly = xEnv)
    lapply(xEnv[[".cloneLast"]], eval, envir = yEnv)
    return(y)
}

.clone_protoContainer <- function(x, exclude_names = c(), ...){
    ## ATTENTION: !!!!
    ## cloning of containers  will *not*  change the parent of the clone
    ## it's your task to change that to point elsewhere in the program
    y <- x
    xEnv <- as.environment(x)
    y@.xData <- yEnv <- new.env(TRUE, parent = parent.env(x))
    x_names <- ls(envir=xEnv, all.names=TRUE)
    x_names <- x_names[!(x_names %in% exclude_names)]
    lapply(x_names, function(nm) assign(nm,
                                        value=clone(xEnv[[nm]], ...),
                                        envir=yEnv))
    y
}

.clone_cellContainer <- function(x, exclude_names = c(), ...){
    ## .cells are not cloned, a new container is always created
    ## tothink:might be necessary for complete replication
    y <- callNextMethod()
    cell_names <- ls(y, all.names = TRUE)
    ## redirect new chells to new prototypes
    lapply(cell_names, function(nm){
        if(!isRoot(y[[nm]])){
            tp <- .type(y[[nm]][[".prototype"]])
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

## tdf <- as.data.frame(.infoForms(pbm$GaNorm))
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
    names1 <- ls(c1, all = T)
    names2 <- ls(c2, all = T)
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
    names1 <- ls(pbm1[[".cells"]], all = T)
    names2 <- ls(pbm2[[".cells"]], all = T)
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
    cat(" Type: \"", .type(object), "\"\n", sep = "")
    methods:::.printNames("All objects: ", ls(objEnv, all.names = TRUE))
    methods:::.printNames("Is Root: ", isRoot(object))
    VL <- 50
    bar <- "\t  --------------------------------  \n"
    cat(" Containers:\n")
    cat(" \n+ Fields:", bar)
    str(.get_all_names_with_host(".fields", objEnv), vec.len = VL)
    ## print(.infoContainer(.get_all_names(objEnv[[".fields"]]), objEnv, ".fields"))
    cat(" \n+ Methods:", bar)
    str(.get_all_names_with_host(".methods", objEnv), vec.len = VL)
    ## print(.infoContainer(.get_all_names(objEnv[[".methods"]]), objEnv, ".methods"))
    cat(" \n+ Forms:", bar)
    str(.get_all_names_with_host(".forms", objEnv), vec.len = VL)
    ## for forms look in objEnv directly:
    ## print(.infoForms(objEnv))
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
    bar <- "\t --------------------------------  \n"
    cat("\n+ Cells:", bar)
    str(.get_all_names_with_host(".cells", objEnv), vec.len = VL)
    cell_names <- ls(objEnv[[".cells"]], all.names = TRUE)
    rev_names <- strsplit(cell_names, ".", fixed = TRUE)
    rev_names <- sapply(rev_names, function(el) paste(rev(el), collapse = "."))
    print(data.frame(` ` = cell_names[order(rev_names)], check.names = FALSE))
}


.show_Container <- function(object){
    callNextMethod()
    methods:::.printNames("Contains: ", ls(as.environment(object), all.names = TRUE))
}

.print_ProtoFormWithEnv <- function(x, code = TRUE, ...){
    ## assumes that "name" exists and checks are not made.
    .print_local <- function(name, where, lev, code){
        if(exists(name, envir = where)){
            form <- get(name, envir = where)
            if(is(form, "protoForm")){
                cat(rep(".. ", lev),"e(", name, ")\n", sep = "")
                if(length(form@doc)) {
                    cat("## DOC: \n")
                    print(form@doc)
                }
            }
            for(i in seq_along(form)){
                fm <- form[[i]]
                if(isECall(fm)){
                    Recall(as.character(fm[[2]]), where, lev = lev + 1L, code)
                }else{
                    if(code){
                        te <- as.expression(fm)[[1]]
                        attr(te, "wholeSrcref") <- NULL ## kludge
                        attr(te, "srcfile") <- NULL ## kludge
                        cat(paste(paste(rep.int("   ", lev), collapse = ""),
                                  capture.output(print(te))),  sep ="\n")
                    }}
            }
        }else{
            cat(rep(".. ", lev),"e(", name, ") <-- missing\n", sep = "")
        }
    }
    ## cat("##", name, "\n", sep = "")
    f_names <- names(x)
    for(i in seq_along(x)){
        fm <- x[[i]]
        if(isECall(fm)){
            .print_local(as.character(fm[[2]]), x@environment, lev = 0L, code)
        }else{
            cat(f_names[[i]], "\n")
            te <- as.expression(fm)[[1]]
            attr(te, "wholeSrcref") <- NULL ## kludge
            attr(te, "srcfile") <- NULL ## kludge
            print(te)
        }
    }
}

.show_ProtoFormWithEnv <- function(object)
    .print_ProtoFormWithEnv(object, code = FALSE)

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
        stop("Form \"", form_name, "\" is not found in the cell of type \"", .type(where), "\"")
    form_browser <- .insertBrowserInForm(form)
    form_browser@hostEnv <- where
    form_browser@isInHost <- exists(form_name, envir = where, inherits = FALSE)
    assign(form_name, form_browser, envir = where)
    invisible(form_name)
}

.undebugForm <- function(form_name, where){
    form <- .getForm(form_name, where)
    if(missing(form))
        stop("Form \"", form, "\" is not found in the cell of type \"", .type(where), "\"")
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
        while(!(identical(tenv, emptyenv()) || is.null(tenv)) && exists("type", envir = tenv)){
            all_names <- c(all_names, ls(tenv, all = T))
            tenv <- parent.env(tenv)
        }
        names <- grep(regexp, unique(all_names), value = TRUE)
    }
    out <- data.frame(row.names = names)
    while(!(identical(env, emptyenv()) || is.null(env)) && exists("type", envir = env)){
        names_in <- names %in% ls(env, all.names = TRUE)
        type <- get("type", envir = env)
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
    all_names <- c()
    while(!identical(containerEnv, emptyenv())){
        all_names <- c(all_names, ls(containerEnv, all.names = TRUE))
        containerEnv <- parent.env(containerEnv)
    }
    unique(all_names)
}


.get_all_names_with_host <- function(container, host){
    "Search recursively for names in 'container', return a list of the form
list(typeA = c('foo', 'bar'), typeB = ...)"
    host <- as.environment(host)
    if(is.character(container))
        container <- host[[container]]
    containerEnv <- as.environment(container)
    all_names <- list()
    while(!identical(containerEnv, emptyenv())){
        all_names[[host[["type"]]]] <- ls(containerEnv, all.names = TRUE)
        containerEnv <- parent.env(containerEnv)
        host <- as.environment(host)[[".prototype"]]
    }
    all_names
}




.infer_type_cell <- function(cell){
    " looks in the .type field if that is --, returns the symbol cell,
if not a symbol, returns NULL"
    stopifnot(is(cell, "protoCell"))
    name <- substitute(cell)
    cellEnv <- as.environment(cell)
    if(identical(cellEnv[["type"]], "--"))
        if(is.name(name)) return(name)
        else return(NULL)
    else return(cellEnv[["type"]])
}

## *******************
.setType <- function(object, type){
    if(is(object, "cellInfo"))
        object[["type"]] <- type
    else if (is(object, "protoCell"))
        assign("type", type, envir = object)
    else
        stop("can not set the type for object of class \"", class(object), "\"")
    object
}

.type <- function(object, fullName = T, collapse = "."){
    if(is(object, "cellInfo")) return(object[["type"]])
    if(is.null(object)) return("NULL")
    if(!is(object, "envProtoClass"))
        stop("The 'object' argument must be of class 'cellInfo' or 'envProtoClass',  suplied an object of class ",  class(object))
    if(fullName){
        type_local <- function(x, type_chain){
            x <- as.environment(x)
            if(isRoot(x)) c(type_chain, x[["type"]])
            else Recall(x[[".prototype"]], c(type_chain, x[["type"]]))
        }
        type <- type_local(object, c())
        if(is.character(collapse))
            paste(type, collapse = collapse)
        else
            type
    }else{
        as.environment(object)[["type"]]
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

.as_cellContainer_graphNEL <- function(from){
    require(graph)
    env_from <- as.environment(from)
    all_names <- ls(env_from, all.names = TRUE)
    leaf_names <- leafNames(from)
    .gr <- new("graphNEL", nodes=all_names, edgemode = "directed")
    ## NODES
    nodeDataDefaults(.gr, "short_name") <- "?"
    nodeDataDefaults(.gr, "type") <- "?" # "leaf" or "prototype"
    nodeData(.gr, all_names, "short_name") <- sapply(all_names, function(nm) get("type", envir = env_from[[nm]]))
    nodeData(.gr, all_names, "type") <- "prototype"
    nodeData(.gr, leaf_names, "type") <- "leaf"
    ## EDGES
    edgeDataDefaults(.gr, "type") <- "?" # "model" or "prototype"
    ## add edges: based only on leaf_names!
    lapply(leaf_names, function(nm){
        rec_names <- unlist(strsplit(nm, split = ".", fixed = TRUE))     ## "aa" "bb" "cc"
        cum_names <-
            Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
                   c(paste(y, x[[1]], sep = "."), x), rev(rec_names), right = FALSE)
        suppressWarnings({
            .gr <<- addEdge(cum_names[ -1], cum_names[-length(cum_names)], .gr)
            edgeData(.gr, cum_names[ -1], cum_names[-length(cum_names)], "type") <<- "prototype"
        })
    })
    .gr
}


## gR <- as(M[[".cells"]], "graphNEL")
## str(renderGraph(layoutGraph(gR)))
## plot(gR)
## edges(gR)

plotCellGraph <-
    function(x, y, col.prototype="cyan",
             col.leafs="yellow",
             layoutType = c("dot","neato","twopi","circo","fdp"),
             plot.root=F, container = ".cells",
             fill = "Set2", #if character of length 1, it is a brewer palette name
             shape = c("circle", "ellipse", "box"),
             col = c("black", "darkgreen", "darkblue"),
             cex = 1,
             lwd = 2,
             textCol = "black",
             bg = "gray20",
             ...){
        x <- as(x, "graphNEL")
        old_par <- par(bg="gray20")
        if(!plot.root){
            x <- subGraph(nodes(x)[!nodes(x)%in%"*"], x)
        }
        localTypeProps <- function(prop, nr, type, names)
            setNames(rep(prop, length.out = nr)[type], names)
        ## rearange common labels such that same type nodes and edges have same graphical properties:
        typeNodes <- unlist(sapply(nodeData(x), function(nd) nd$type))
        typeEdges <- unlist(sapply(edgeData(x), function(nd) nd$type))
        common <- intersect(typeNodes, typeEdges)
        all_labels <- unique(c(typeEdges, typeNodes))
        all_labels <- c(common, setdiff(all_labels, common))
        typeNodes <- factor(typeNodes, levels = all_labels)
        typeEdges <- factor(typeEdges, levels = all_labels)
        ## NODES
        type <- typeNodes
        labels <- sapply(nodeData(x), function(nd) nd$short_name)
        nodeNames <- names(type)
        if(is.character(fill) && length(fill) == 1L){
            library(RColorBrewer)
            fill <- brewer.pal(max(length(levels(type)), 3L), fill)
        }
        nrt <- length(levels(type))
        nodeRenderInfo(x) <-
            list(lwd = localTypeProps(lwd, nrt, type, nodeNames),
                 cex = localTypeProps(cex, nrt, type, nodeNames),
                 textCol = localTypeProps(textCol, nrt, type, nodeNames),
                 col = localTypeProps(col, nrt, type, nodeNames),
                 fill = localTypeProps(fill, nrt, type, nodeNames),
                 shape = localTypeProps(shape, nrt, type, nodeNames),
                 label = labels)
        ## EDGES
        type <- typeEdges
        edgeNames <- names(type)
        names(type) <- edgeNames <- gsub("\\|", "~", edgeNames)
        ## if(is.character(fill) && length(fill) == 1L)
        ##     fill <- brewer(max(length(levels(type)), 3L), fill)
        edgeRenderInfo(x) <-
            list(col = localTypeProps(fill, nrt, type, edgeNames))
        ## LAYOUT
        layoutType <- layoutType[[1]]
        x <- layoutGraph(x, layoutType=layoutType)
        renderGraph(x)
        par(old_par)
    }


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
        type <- get("type", envir = pObject)
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
        info <- c(cellType = x[["type"]],
                  homeCxtType = .type(x[[".self"]][[".homeContext"]]),
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
        info <- c(contextType = x[["type"]],
                  class = class(x[[".self"]]))
        if(isRoot(x)) rbind(info, info_chain)
        else Recall(x[[".prototype"]], rbind(info, info_chain))
    }
    out <- info_local(object, data.frame(contextType = "--",
                                         class = "--"))
    out[-nrow(out), ]
}


.infoForms <- function(pObject, names = NULL, regexp = NULL, all.names = FALSE, expandCode = FALSE) {
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
            names[names %in% ls(pObject, all = TRUE)]
        }
    res <- replicate(length(names),
                     data.frame(check.names = FALSE), simplify = FALSE)
    names(res) <- names
    while(!is.null(pObject)){
        type <- get("type", envir = pObject)
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

browseInfo <-
    function (x, File, title = "Info Browser", header1 = "", header2 = "",
              openBrowser = TRUE, stylesheet = "info.dark", ...)
{
    ## x should be a df.
    library(brew)
    library(highlight)
    if (nrow(x) < 1) {
        cat("x has zero rows;  nothing to display.\n")
        if (missing(where))
            where <- ""
        return(invisible(where))
    }
    if (missing(File))
        f0 <- tempfile()
    for (i in 1:111) {
        File <- paste(f0, ".html", sep = "")
        fInf <- file.info(File)
        if (all(is.na(fInf)))
            break
        f0 <- paste(f0, "1", sep = "")
    }
    Dir <- dirname(File)
    if (Dir == ".") {
        Dir <- getwd()
        File <- file.path(Dir, File)
    }else{
        dc0 <- dir.create(Dir, FALSE, TRUE)
    }
    js <- file_path_as_absolute("~/works/R_dev/pbm/info.js")
    css <-
        switch(stylesheet,
               info.dark = file_path_as_absolute("~/works/R_dev/pbm/info.dark.css"),
               file_path_as_absolute("~/works/R_dev/pbm/info.css")
               )
    if (!file.exists(js)) {
        stop("Unable to locate '", js, "' file")
    }else if(!file.exists(css)){
        stop("Unable to locate '", css, "' file")
    }else {
        file.copy(js, Dir, overwrite = TRUE)
        file.copy(css, Dir, overwrite = TRUE)
    }
    templateFile <- "~/works/R_dev/pbm/template.html"
    if (!file.exists(templateFile))
        stop("Unable to locate '~/works/R_dev/pbm/template.html' file")
    template <- file(templateFile, encoding = "utf-8", open = "r")
    xenv <- new.env()
    assign("header1", header1, envir = xenv)
    assign("header2", header2, envir = xenv)
    assign("x", x, envir = xenv)
    assign("title", title, envir = xenv)
    assign("stylesheet", basename(css))
    brew(template, File, envir = xenv)
    close(template)
    FileInfo <- file.info(File)
    if (is.na(FileInfo$size) || FileInfo$size <= 0) {
        if (is.na(FileInfo$size)) {
            stop("Brew did not create file ", File)
        }
        else {
            stop("Brew created a file of size 0")
        }
    }else if (openBrowser) {
        if (is.na(FileInfo$size)) {
            warning("Did not create file ", File, ";  nothing to give to a browser.")
        }
        else {
            if (FileInfo$size <= 0) {
                warning("0 bytes in file ", File, ";  nothing to give to a browser.")
            }
            else {
                browseURL(File)
            }
        }
    }
    invisible(File)
}

browseForms <-
    function(pObject, names = NULL, regexp = NULL, all.names = FALSE, stylesheet = "info.dark", ...) {
        x <- as.data.frame(.infoForms(pObject = pObject, names = names, regexp = regexp,
                                      all.names = all.names))
        title <- gettextf("Form info for %s of type \" %s\"",
                          class(pObject), .type(pObject))
        header1 <- title
        header2 <- paste("Info for the following names: \n",
                         paste(names, collapse = ", "))
        browseInfo(x, title = title, header1 = header1, header2 = header2, stylesheet = stylesheet, ...)
    }

source("../R/classes.R")
