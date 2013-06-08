###_ CLASS

##' Class to represent proto contexts
##' @export
setClass("protoContext", contains = "envProtoClass")

##' Constructor for protoContext objects
##'
##' @inheritParams protoCell
##' @param cells ...
##' @param initCells ...
##' @param rootParentEnv ...
##' @return an object of class protoContext
##' @author Vitalie Spinu
protoContext <- function(type = "--",
                         prototype = NULL,
                         initFields = list(),
                         initMethods = list(),
                         initForms = list(),
                         initCells = list(),
                         setFields = list(),
                         setMethods = list(),
                         setForms = list(),
                         setCells = list(),
                         expr = expression(),
                         rootParentEnv = NULL, ## todo:  make field
                         ...){
    new("protoContext", type = type, prototype = prototype,
        initCells = initCells, setCells = setCells,
        initMethods = initMethods, setMethods = setMethods,
        initFields = initFields, setFields = setFields,
        initForms = initForms, setForms = setForms,
        expr = expr, ...)}

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
              cell_names <- cell_names[order(rev_names)]
              print(data.frame(` ` = cell_names,  
                               ` ` = sapply(cell_names, USE.NAMES = F, 
                                 function(nm) format(as.environment(get(nm, objEnv[[".cells"]])))),
                               check.names = FALSE))
              cat("\n")
          })

##' @rdname dollar
setMethod("$", signature(x = "protoContext"),
          function(x, name){
              if(!is.null(obj <- .getCell(name, as.environment(x))))
                  return(obj)
              else
                  callNextMethod()
          })

##' @rdname dollar
setMethod("$<-", signature(x = "protoContext"),
          function(x, name, value){ # must follow dollar arguments, dont realy like this :(
              .cells <- get(".cells", envir = x)
              match <- .complete_partial_name(name, .cells)
              if(is(value, "protoCell") && !(is.na(match) || match == 0L)){
                  assign(match, value, envir = .cells)
              }else{
                  callNextMethod(x, name, value)
              }
              invisible(x)
          })

.DollarNames.protoContext <- function(x, pattern = ""){
    c(.DollarNames.envProtoClass(x, pattern),
      .get_all_names(get(".cells", envir = x, inherits = F), exclude_special = F))
}
## registerS3method(".DollarNames", "protoContext", .DollarNames.protoContext)

setMethod("setPrototype", "protoContext",
          function(protoObj, prototype){
              callNextMethod()
              parent.env(get(".cells", envir = protoObj)) <- parent.env(get(".cells", envir = prototype))
          })


##' Return TRUE is it's a default context
##'
##' @param envProtoObj Object from a subclass of envProtoClass
##' @author Vitalie Spinu
isDefaultContext <- function(envProtoObj)
    exists(.defaultMetaName, envir = as.environment(envProtoObj), inherits = FALSE)




###_ INITIALIZE
..eloadco0 <- expression({
    setMethod("initialize", signature(.Object = "protoContext"),
              function(.Object,
                       prototype = NULL,
                       initCells = list(),
                       setCells = list(),
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
                  if(length(setCells))
                      .generic_setter(setCells, .Object, ".cells")
                  .Object
              })
})
evalOnLoad(..eloadco0)

setMethod("initializeRoot", "protoContext",
          function(.Object,
                   initCells = list(), ...){
              .Object <- callNextMethod(.Object, ...) ##envProtoClass
              objEnv <- as.environment(.Object)
              objEnv[[".cells"]] <-  new("cellContainer", host = .Object) ## empty parent by default
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
                                }),
                           where = objEnv)
              
              .initFields(list(cells = protoContainerField(".cells"),
                               rootCellParentEnv = protoField(".rootCellParentEnv")), 
                          where = objEnv)
              objEnv[[".rootCellParentEnv"]] <- globalenv() # parent of the .rootCell

              .Object
          })



###_ CLASS REPRESENTATION
.signAsDefaultContext <- function(envProtoObj)
    assign(.defaultMetaName, TRUE, envir = as.environment(envProtoObj), inherits = FALSE)

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
                            cellClass = "protoCell",
                            contains = "protoContext",
                            where = topenv(parent.frame()),
                            ...){

    which_pc <- sapply(contains, extends, "protoContext")
    ## temporarily assign an ordinary class definition
    if(any(which_pc))
        cls <- contains[[which_pc]][[1]]
    else
        stop("none of the classes in 'contains' argument inherit from 'protoContext' class")

    defaultContext <- new(cls, type = paste(Class, "@",  sep = ""),
                          prototype = getClassDef(cls)@defaultContext)
    .signAsDefaultContext(defaultContext)

    setClass(Class, contains = contains, where = where, ...)
    ## generate contextClassRepresentation definition
    classDef <- new("contextClassRepresentation",
                    getClassDef(Class, where = where),
                    defaultContext = defaultContext,
                    cellClass = cellClass)
    
    classDef <- .modifyAs(classDef)
    assignClassDef(Class, classDef, where, force = force)
    
    ## default context should be also of class "Class"
    classDef@defaultContext <- as(defaultContext, Class)
    assignClassDef(Class, classDef, where, force = force)
    
    Class
}

setClass("contextClassRepresentation",
         representation(defaultContext = "protoContext",
                        cellClass = "character"),
         prototype = list(cellClass = "protoCell"),
         contains = "classRepresentation")

setMethod("show", "contextClassRepresentation",
          function(object){
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
          })


..eloadco1 <- expression({
    assignClassDef("protoContext",
                   .modifyAs(new("contextClassRepresentation", getClassDef("protoContext") ,
                                 defaultContext = newRoot("protoContext", type = "@", isDefaultContext = TRUE))
                             ))
    ## Install the super-root cell in the super - root default context
    getClassDef("protoContext")@defaultContext$initCells(`*` = newRoot("protoCell"))
})

eval(..eloadco0)
eval(..eloadco1)
evalOnLoad(..eloadco0)
evalOnLoad(..eloadco1)
