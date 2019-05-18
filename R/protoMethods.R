###_ CLASS
setClass("protoMethod",
         representation(bindName = "character",
                        container = "character"),
         ## todo: change to typeContainer
         prototype(container = ".methods"),
         contains = "protoFunction")

protoMethod <- function(fun = function() NULL, doc = "", ...){
    new("protoMethod", fun, doc = doc, ...)
}


###_ CONTAINER
setClass("methodContainer",
         prototype = prototype(typeContainer = ".methods"),
         contains = "protoContainer")

setMethod("$", signature(x = "methodContainer"),
          .dollarGet_methodContainer <- function(x, name){
              meth <- get(name, envir = x)
              environment(meth) <- x@host
              meth
          })

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

setMethod("$<-", signature(x = "methodContainer"),
          function(x, name, value) .dollarSet_methodContainer(x, name, value))

.setMethod <- function(x, name, value, error = TRUE)
    ## x is protoEnv
    .dollarSet_methodContainer(get(".methods", envir = x), name, value, error)

setMethod("specialNames", "methodContainer",
          function(protoObject)
          c("initCells", "initFields", "initForms", "initMethods",
            "setFields", "setForms", "setMethods",
            "eval", "evalq", "new", 
            "inspect", "debug", "undebug"))

## .existsMethod <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".methods"]])
## }

.getMethod <- function(name, selfEnv){
    if(exists(name, envir = selfEnv[[".methods"]])){
        .dollarGet_methodContainer(selfEnv[[".methods"]], name)
    }else{
        substitute()
    }
}



###_ INITIALIZE
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
        ## default method
        installBinding(method, whereEnv[[".methods"]], methodNames[[i]])
    }
    invisible(methodNames)
}
