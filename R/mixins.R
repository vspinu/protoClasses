## Should end up as a formal mixin implementation
##
## Should constitute the default hierarchy of bcells. Sampling and adaptation
## algorithms should be in mixin containers. Exactly the opposite of what is
## now.

setClass("protoMixin",
         c(mixins = "list", subtype = "character"), 
         contains = "namedList")

mixin <- function(..., parent_mixins = list(), subtype = character()){
    if(is(parent_mixins, "protoMixin"))
        parent_mixins <- list(parent_mixins)
    good <- sapply(parent_mixins, function(m) is(m, "protoMixin") || is.name(m))
    if(any(!good))
        stop("arguments 'mixins' should contain only mixins or symbols")
    new("protoMixin", list(...), mixins = parent_mixins, subtype = subtype)
}

.mix_in <- function(mixins, out_list = list(), name = NULL){
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
                   c(.mix_in(mx@mixins, name = name), mx[[name]])
               }),
        recursive = F),
             out_list)
    nulls <- sapply(out, is.null)
    out[!nulls]
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
