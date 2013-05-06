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



.debugObjects <- function(names, .methods = c(), .fields = c(), .forms = c(), .where){
    names <- unlist(names)
    for( nm in names)
        if(exists(nm, .where[[".fields"]]))
            .fields <- c(nm, .fields)
        else if(exists(nm, .where[[".methods"]]))
            .methods <- c(nm, .methods)
        else if(exists(nm, .where[[".forms"]]))
            .forms <- c(nm, .forms)
    for(nm in .methods)
        .mark_for_debug(nm, .where[[".methods"]], "method")
    for(nm in .fields)
        .mark_for_debug(nm, .where[[".fields"]], "field")
    for(nm in .forms)
        .mark_for_debug(nm, .where[[".forms"]], "form")
}

.undebugObjects <- function(names, .methods = c(), .forms = c(), .fields = c(), .where){
    if(length(names) == 0 && length(.methods) == 0 && length(.forms) == 0 && length(.fields) == 0)
        stop("undebuggin all objects is not implemlented yet")
    names <- unlist(names)
    for( nm in names)
        if(exists(nm, .where[[".fields"]]))
            .fields <- c(nm, .fields)
        else if(exists(nm, .where[[".methods"]]))
            .methods <- c(nm, .methods)
        else if(exists(nm, .where[[".forms"]]))
            .forms <- c(nm, .forms)
    for(nm in .methods)
        .unmark_for_debug(nm, .where[[".methods"]])
    for(nm in .fields)
        .unmark_for_debug(nm, .where[[".fields"]])
    for(nm in .forms)
        .unmark_for_debug(nm, .where[[".forms"]])
}

.funcWithBrowser <- function(func){
    b <- quote({br;body})
    attributes(b) <- NULL
    b[[2]] <- quote(browser())
    b[[3]] <- body(func)
    body(func) <- b
    func
}

.mark_for_debug <- function(obj_name, where, type = "form"){
    if(!exists(obj_name, where))
        stop(type, " \"", obj_name, "\" is not found in the cell of type \"", .getType(where), "\"")
    obj <- get(obj_name, where)
    if(is(obj, "protoObjectWithBrowser"))
        warn.pbm("Object \"", obj_name, "\" is already marked for debugging; skipping.")
    else{
        obj_wbr <-
            switch(type,
                   form = new("protoFormWithBrowser", c(expression(browser = browser()), original = form)), 
                   method = new("protoMethodWithBrowser", .funcWithBrowser(obj)),
                   field = new("protoFieldWithBrowser", .funcWithBrowser(obj)), 
                   stop("unrecognised type, should be one of form, method or field."))
        obj_wbr@original <- obj
        obj_wbr@hostEnv <- where
        obj_wbr@isInHost <- exists(obj_name, envir = where, inherits = FALSE)
        assign(obj_name, obj_wbr, envir = where)
    }
    invisible(obj_name)
}

.unmark_for_debug <- function(obj_name, .where){
    obj <- get(obj_name, .where, inherits = FALSE)
    remove(list = obj_name, envir = .where)
    if(obj@isInHost)
        assign(obj_name, obj@original, envir = .where)
}

## .debugForm <- function(form_name, where){
##     form <- .getForm(form_name, where)
##     if(missing(form))
##         stop("Form \"", form_name, "\" is not found in the cell of type \"", .getType(where), "\"")
##     form_browser <- .insertBrowserInForm(form)
##     form_browser@hostEnv <- where
##     form_browser@isInHost <- exists(form_name, envir = where, inherits = FALSE)
##     assign(form_name, form_browser, envir = where)
##     invisible(form_name)
## }

## .undebugForm <- function(form_name, where){
##     form <- .getForm(form_name, where)
##     if(missing(form))
##         stop("Form \"", form_name, "\" was not found in cell \"", .getType(where), "\"")
##     if(is(form, "protoFormWithBrowser")){
##         remove(list = form_name, envir = form@hostEnv)
##         if(form@isInHost)
##             assign(form_name, form@original, envir = form@hostEnv)
##     }else{
##         stop("form \"", form_name, "\"is not marked for debugging")
##     }
## }
