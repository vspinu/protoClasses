## Should end up as a formal mixin implementation
##
## Should constitute the default hierarchy of bcells. Sampling and adaptation
## algorithms should be in mixin containers. Exactly the opposite of what is
## now.

setClass("protoMixin",
         c(mixins = "list", subtype = "character"), 
         contains = "namedList")

mixin <- function(..., parentMixins = list(), subtype = character()){
    if(is(parentMixins, "protoMixin"))
        parentMixins <- list(parentMixins)
    good <- sapply(parentMixins, function(m) is(m, "protoMixin") || is.name(m))
    if(any(!good))
        stop("arguments 'mixins' should contain only mixins or symbols")
    dots <- list(...)
    if(any(dups <- duplicated(names(dots))))
        stop(sprintf("%s occurs multiple times.", names(dots)[dups][[1]]))
    new("protoMixin", list(...), mixins = parentMixins, subtype = subtype)
}

.mix_in_1 <- function(mixins, out_list = list(), name = NULL){
    ## Merge all the list with name NAME from mixins into out_list
    ## mixins is a list of mixins or a mixin;
    if(length(mixins) == 0L)
        return(out_list)
    
    if(is(mixins, "protoMixin"))
        mixins <- list(mixins)

    if(is.null(name))
        ## guess name
        name <- as.character(as.name(substitute(out_list)))

    out <- c(unlist(
        lapply(rev(mixins),
               function(mx){
                   if(is.name(mx))
                       mx <- eval(mx)
                   if(!(is(mx, "list")))
                       stop(sprintf("trying to mix in an object of class %s, whereas 'protoMixin' is required",
                                    class(mx)))
                   ## recursively call on parent mixins
                   c(.mix_in_1(mx@mixins, name = name), mx[[name]])
               }),
        recursive = F),
             out_list)
    nulls <- sapply(out, is.null)
    out[!nulls]
}

.mixin <- function(mixins, .Object, initMethods = list(),
                   initFields = list(), initForms = list(), setMethods = list(),
                   setFields = list(), setForms = list(), expr = list()){

    ## fixme: type might be changed by subtypes but the cell assigned in the
    ## container is still by old name :(
    for(nm in c("initForms", "setForms", "initFields",
                "setFields", "initMethods", "setMethods", "expr"))
        eval(substitute(nm <- .mix_in_1(mixins, nm), list(nm = as.name(nm))))

    assign(".subtypes",
           c(.get_subtypes(mixins), get(".subtypes", .Object)),
             .Object)

    .initFields(initFields, .Object)
    .initMethods(initMethods, .Object)
    .initForms(initForms, .Object)
    
    if(length(setFields))
        .generic_setter(setFields, .Object, ".fields")
    if(length(setMethods))
        .generic_setter(setMethods, .Object, ".methods")
    if(length(setForms))
        .generic_setter(setForms, .Object, ".forms")

    ## fixme: this sucks, prototype at this point the object in default context,
    ## not the newlly instantiated cell in the container. Thus rootParentEnv is
    ## not accessible. It looks like the only way to solve it is to catch expr
    ## in initialize of protoCell, then BCell etc ... bad bad bad
    if(is.list(expr)) 
        for(e in expr)
            eval(e, envir = .Object)
    else
        eval(expr, envir = .Object)
    invisible(NULL)
}
    

.get_subtypes <- function(mixins){
    if(is(mixins, "protoMixin"))
        mixins <- list(mixins)
    
    unlist(lapply(mixins,
                  function(x)
                  if(is(x, "protoMixin")){
                      c(x@subtype, .get_subtypes(x@mixins))
                  }))
}
