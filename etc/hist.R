
## if(existsMethod("initialize", "envProtoClass"))
##     removeMethod("initialize", "envProtoClass")
## if(existsMethod("initialize", "protoContext"))
##     removeMethod("initialize", "protoContext")
## if(existsMethod("initialize", "protoCell"))
##     removeMethod("initialize", "protoCell")

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

## .getOrCreateForm <- function(bindName, whereEnv)
##     if(exists(bindName, envir = get(".forms", envir = whereEnv))){
##         get(bindName, envir = whereEnv)
##     }else{
##         new("protoForm")
##     }

## .replaceDots <- function(names){
##     if(length(dot_here <- grep(".", names, fixed = TRUE))){
##         warning("\".\" was replaced with \"_\" in ",
##                 paste(names[dot_here], collapse=", "))
##         names <- gsub(".", "_", names, fixed = TRUE)
##     }
##     names
## }


## areIdenticalPBM <- function(pbm1, pbm2){
##     names1 <- ls(pbm1[[".cells"]], all.names =  T)
##     names2 <- ls(pbm2[[".cells"]], all.names =  T)
##     reserved <- character()
##     if(length(diff1 <- setdiff(setdiff(names1, names2), reserved)))
##         message("folowing cells are found in pbm1 and  not in pbm2: \n",
##                 paste(diff1, collapse = ", "))
##     if(length(diff2 <- setdiff(setdiff(names2, names1), reserved)))
##         message("folowing cells are found in pbm2 and not in pbm1: \n",
##                 paste(diff2, collapse = ", "))
##     if(length(diff1) == 0 && length(diff2) == 0){
##         diffs <- sapply(names1, function(nm) {
##             cat(" --> ", nm, "\n")
##             areIdentical(pbm1[[".cells"]][[nm]], pbm2[[".cells"]][[nm]])
##         })
##         diffs
##     }else{
##         TRUE
##     }
## }


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
