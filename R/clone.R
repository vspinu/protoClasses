### CLONE functionality is in a separate file because it is likely to be amended
### often, and changes usually affect all methods.

setGeneric("clone",
           def = function(x, ...) standardGeneric("clone"),
           useAs = function(x, ...) x)

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

setMethod("clone", "envProtoClass",
          function(x, exclude_names = c(), ...){
              ## EXCLUDE_NAMES are not cloned or copied (ex. .cells in protoContext class)
              ## TOTHINK: environments of ALL functions is changed here to point to new environment
              y <- x
              xEnv <- as.environment(x)
              y@.xData <- yEnv <- new.env(TRUE, parent = parent.env(xEnv))
              ## lapply(xEnv[[".cloneFirst"]], eval, envir = yEnv)
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
              ## lapply(xEnv[[".cloneLast"]], eval, envir = yEnv)
              return(y)
          })

setMethod("clone", "cellContainer", 
          function(x, exclude_names = c(), ...){
              ## .cells are ???not???? cloned, a new container is always created
              ## tothink:might be necessary for complete replication
              y <- callNextMethod()
              cell_names <- ls(y, all.names = TRUE)
              ## redirect new chells to new prototypes (empty always?)
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
          })

setMethod("clone", "protoContext",
          function(x, exclude_names = c(), clone.cells = FALSE, ...){
              exclude_names <- unique(c(exclude_names, ".rootCellParentEnv"))
              y <- callNextMethod(x, exclude_names = exclude_names, ...)
              y[[".rootCellParentEnv"]] <- x[[".rootCellParentEnv"]]
              ## change homeContext of cells to y
              cells <- y[[".cells"]]
              cell_names <- ls(cells, all.names = TRUE)
              lapply(cell_names, function(nm){
                  cells[[nm]][[".homeContext"]] <- y
              })
              y
          })

setMethod("clone", "protoContainer",
          function(x, exclude_names = c(), ...){
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
          })
