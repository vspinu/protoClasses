###_ CLASS

##' Class to represent cells.
##'
##' See \link{protoCell} constructor for more details.
##' @export
setClass("protoCell", contains = "envProtoClass")
setMethod("initializeRoot", "protoCell",
          function(.Object,
                   homeContext = NULL,  ...){
              .Object <- callNextMethod(.Object, ...) ##envProtoClass
              list2env(list(.homeContext = homeContext),
                       envir = as.environment(.Object))
              .Object
          })

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
protoCell <- function(type = "--",
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

setMethod("installBinding", "protoCell",
          function(bindDefinition, container, bindName, ...){
              ## bindName has precedence:
              if(!missing(bindName) && !identical(bindName, ""))
                  assign(".type", bindName, envir = bindDefinition)
              else
                  bindName <- get(".type", envir = bindDefinition)
              .installCellInContainer(bindDefinition, container = container)
          })



###_ CELL DEFINITION
setClass("protoCellDefinition",
         representation(cellClass = "character",
                        names = "character"),
         prototype(cellClass = "protoCell"),
         contains = "namedList")

setMethod("installBinding", "protoCellDefinition",
          function(bindDefinition, container, bindName, ...){
              if(!missing(bindName) && !identical(bindName, ""))
                  bindDefinition[["type"]] <- bindName
              new_cell <- cellFromDefinition(bindDefinition, homeContext = container@host)
              installBinding(new_cell, container, bindName) ## fixme: bindName is not needed here?
              invisible(new_cell)
          })

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

defC <- function(...){
    args <- list(...)
    args[["Class"]] <- NULL
    new("protoCellDefinition", args, cellClass = "protoCell")
}



###_ CONTAINER
setClass("cellContainer",
         prototype = prototype(typeContainer = ".cells"),
         contains = "protoContainer")

setMethod("$", signature(x = "cellContainer"),
          function(x, name) .getPartial(name, x))

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

.getCell <- function(name, selfEnv){
    .cells <- as.environment(selfEnv[[".cells"]])
    .getPartial(name, .cells, trigger_error = FALSE)
}

.setCell <- function(x, name, value, error = TRUE){
    stop("Not implemented yet")
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

## .existsCell <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".cells"]])
## }
## .existsPartialCell <- function(name, selfEnv){
##     .existsPartial(name, selfEnv[[".cells"]])
## }

.setHomeContext <- function(cell, context)
    assign(".homeContext", context, as.environment(cell))

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



###_ CLASS REPRESENTATION
setClass("cellClassRepresentation",
         representation(contextClass = "character"),
         ## really need it?
         prototype = list(contextClass = "protoContext"),
         contains = "classRepresentation")
assignClassDef("protoCell",
               .modifyAs(new("cellClassRepresentation", getClassDef("protoCell"))))


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



###_ INITIALIZE
setMethod("initialize", signature(.Object = "protoCell"),
          function(.Object,
                   prototype = "*",
                   type = "--",
                   homeContext = NULL,
                   ## setFoo and initFoo are doen in envProtoClass next method
                   ...){
              ## PROTOTYPE can be a character string or a valid envProtoObject
              ## CELL is *NOT* installed in the context (it's the task of initCell interface)
              ## Cannot create the root cell!! supplying * as TYPE produces the cell *.*
 
              ## CONTEXT
              context <-
                  if(is.null(homeContext))
                      ##default context
                      getClassDef(getClassDef(class(.Object))@contextClass)@defaultContext
                  else as.environment(homeContext)[[".self"]]

              isValidProtoObject(context, trigger_error = TRUE, object_name = "context")
              ## if homeContext = NULL next check is superfluous
              if(!is(context, tCls <- getClassDef(class(.Object))@contextClass))
                  stop(gettextf("Attempt to create an object of class %s in a homeContext of class %s
that doesn't extend cell's default context class %s",
                                class(.Object), class(homeContext), tCls))

              
              ## PROTOTYPE
              .cells <- as.environment(get(".cells", context))
              if(is.character(prototype)){
                  prototype <- .getPartial(prototype, container = .cells,
                                           trigger_error = TRUE, object_name = "prototype")
              }

              isValidProtoObject(prototype, trigger_error = TRUE, object_name = "prototype")
              
              type <- as.character(type)

              ## CREATE THE CELL
              .Object <- callNextMethod(.Object, type = type, prototype = prototype,  ...)

              ## Special OBJECTS:
              objEnv <- as.environment(.Object)
              objEnv[[".homeContext"]] <- homeContext
              objEnv[[".self"]] <- .Object

              .Object
          })

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
