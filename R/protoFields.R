###_ CLASS
setClass("protoField",
         representation(bindName = "character",
                        installedObjectNames = "character",
                        className = "character",
                        container = "character"),
         prototype(container = ".fields",
                   bindName = "dummy_field"),
         contains = "protoFunction")

protoField <- function(func = function(value) NULL ,  doc = "", ...){
    ## if string
    if(is.character(func)){ 
        func <- eval(substitute(function(value){
            if(missing(value)) get(nm, .self)
            else assign(nm, value, .self)
        }, list(nm = func)))
    } else {
        ## if function
        subfunc <- substitute(func)
        if(getOption("protoClasses.debugMode", FALSE) &&
           is.name(subfunc) && is.function(func))
            func <- eval(substitute(
              function(value){
                  loc_func <- func_name
                  environment(loc_func) <- .self
                  loc_func(value)
              }, list(func_name = subfunc)))
    }
    new("protoField", func, doc = doc, ...)
}

protoReadOnlyField <- function(object_name, replace_leading_dot = TRUE)
    protoField(eval(substitute(
      function(obj){
          if(missing(obj)){
              get(obj_name, .self)
          }else{
              if(rep)
                  obj_name <- sub("^\\.", "", obj_name)
              stop("field '", obj_name, "' is readonly")
          }
      }, list(obj_name = object_name, rep = replace_leading_dot))))

protoContainerField <- function(container_name){
    protoField(eval(substitute(
      function(obj){
          if(missing(obj)){
              get(container_name, .self)
          }else{
              if(!is(obj, "protoContainer"))
                  stop("assigned value should be a container (", container_name, ")")
              if(!identical(as.environment(obj@host), as.environment(.self)))
                  stop("invalid host of the container, (probably a bug, please report)")
              assign(container_name, obj, .self)
          }
      })))
}


###_ CONTAINER
setClass("fieldContainer",
         prototype = prototype(typeContainer = ".fields"),
         contains = "protoContainer")

setMethod("installBinding", "protoField",
          function(bindDefinition, container, bindName, ...){
              ## assign if different
              ## containerEnv <- get(container, envir = where)
              ## if(exists(bindName, envir = containerEnv)){
              ##     oldField <- get(bindName, envir = containerEnv)
              ##     ## if(!identical(bindDefinition, oldField))
              ##     ##     callNextMethod()
              ##     ## ## else do nothing
              ## }else{
              callNextMethod()
          })

.dollarGet_fieldContainer <- function(x,name){
    field_fun <- get(name, envir = x)
    environment(field_fun) <- x@host
    field_fun()
}
setMethod("$", signature(x = "fieldContainer"),.dollarGet_fieldContainer)

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

setMethod("$<-", signature(x = "fieldContainer"),
          function(x, name, value) .dollarSet_fieldContainer(x, name, value))

setMethod("specialNames", "fieldContainer",
          function(protoObject) c("f", "h", "m", "type", "Type"))

## .existsField <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".fields"]])
## }

.getField <- function(name, selfEnv, error = FALSE)
    if(exists(name, envir = selfEnv[[".fields"]])){
        .dollarGet_fieldContainer(selfEnv[[".fields"]], name)
    }else{
        substitute()
    }



###_ INITIALIZE
setMethod("initialize", signature(.Object = "protoField"),
          function(.Object,  ...){
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
          })


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
