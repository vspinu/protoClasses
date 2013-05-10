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

###_ GUIDE
## Rely only on internal implementation.
## Always use as.environment (do not rely on [[]], or other user changeable methods)
## All this funcs start with .
## Do not rely on  objects methods (like $, methods, etc) (only func programming)


###_ CLASSES
setClass("protoFunction",
         representation(changeCallEnv = "logical",
                        doc = "character"),
         prototype(changeCallEnv = FALSE),
         contains = "function")


###_ METHODS:
.installBinding_default <- function(bindDefinition, container, bindName, ...){
    ## default methods just assigns the stuff in the container.
    assign(bindName, bindDefinition, envir = container)
}
setGeneric("installBinding",
           def = function(bindDefinition, container, bindName, ...) standardGeneric("installBinding"),
           useAsDefault = .installBinding_default)

setGeneric("initializeRoot",
           function(.Object, ...) standardGeneric("initializeRoot"))

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


## these names are excluded on clonning
setGeneric("specialNames", def = function(protoObject) standardGeneric("specialNames"),
           useAsDefault = function(protoObject) character())



###_ ACCESSORS
## create_specialised_accesor <- function(type){
##     fun <- eval(substitute(
##       function(value){
##           if(missing(value))
##               container_name
##           else{
##               if(!is(value, "environment") ||
##                  !identical(as.environment(container_name), as.environment(value)))
##                   warning("Oops, trying to assing non-native container. Are you messing with internals through user interface?")
##           }
##       }, list(container_name = as.name(type))))
##     attributes(fun) <- NULL
##     fun
## }

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
    ## outclass <-
    ##     switch(container_name,
    ##            .fields = "protoFieldSET",
    ##            .forms = "protoFormSET", 
    ##            .methods = "protoMethodSET", 
    ##            .cells = "protoCellSET")
    .extract <-
        function(names, selfEnv){
            lapply(names, function(nm){
                out <- getFUN(nm, selfEnv)
                if(missing(out))
                    stop("Can not find object \"", nm, "\"")
                else
                    out
            })}
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
        setNames(.extract(names, selfEnv), names = names)
    }
}



###_ CLASS REPRESENTATIONS:
## Extension of classRepresentation to store two new slots: defaultContext and
## classDef. each new protoContext class generates new classDef with it's
## default context.
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



###_ UTILS

##' Return TRUE if envProtoObj is a root object
##'
##' @param envProtoObj Object from a subclass of envProtoClass
##' @author Vitalie Spinu
##' @export
isRoot <- function(envProtoObj)
    exists(.rootMetaName, envir = as.environment(envProtoObj), inherits = FALSE)

newRoot <- function(Class, ...){
    ClassDef <- getClass(Class, where = topenv(parent.frame()))
    .Object <- .Call(methods:::C_new_object, ClassDef)
    .Object@.xData <- new.env(TRUE, parent = topenv(parent.frame()))
    initializeRoot(.Object, ...)
}

isMirror <- function(obj)
    exists("._mirror", obj, inherits = F) && get("._mirror", obj)

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



###_ INTERNALS
.rootMetaName <- ".|.root"
.defaultMetaName <- ".|.defaultContext"

.gen_mirr_name <- function(mname)
    paste0("_", mname)

.signAsRoot <- function(envProtoObj)
    assign(.rootMetaName, TRUE, envir = as.environment(envProtoObj), inherits = FALSE)


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
    ## prints the hierarchy of changed objects in the parents of env
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

.get_form_host <- function(cell, form_name){
    "Get the form's host cell. No checks. return empty env if not found"
    cellEnv <- as.environment(cell)
    while(!(exists(form_name, cellEnv, inherits = F) || identical(cellEnv, emptyenv()))){
        cellEnv <- parent.env(cellEnv)
    }
    cellEnv[[".self"]]
}



###_ ONLOAD
.onLoad  <- function(libname, pkgname){
    where <- environment(sys.function())  # the namespace

    options(protoClasses.changeCallEnv = FALSE,
            protoClasses.debugMode = TRUE,
            protoClasses.print_expanded_forms = FALSE)
}

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

## for(nm in c(
##             "envProtoClass.R", 
##             "protoFields.R", 
##             "protoForms.R", 
##             "protoMethods.R", 
##             "protoCells.R", 
##             "protoContexts.R", 
##             "clone.R", 
##             "debug.R", 
##             "graph.R", 
##             "info.R"))
##     source(nm)
