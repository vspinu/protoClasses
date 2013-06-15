###_ CLASS

##' Class to represent protoForms.
##'
##' protoForms are expressions which can contain protoForms as
##' subexpressions. Slots are:
##' @export
##' @slot names character vector providing names for each subexpression or
##' subform.
##' @slot doc Self contained documentation.
setClass("protoForm",
         representation(names = "character",
                        doc = "character"),
         ## level = "numeric",
         ## cell = "environment"),
         contains = "expression")

form <- function(..., doc = character()){
    ## All this storry preserves source references
    mc <- match.call()
    mc$doc <- NULL
    mc[[1]] <- as.name("expression")
    expr <- eval(mc)
    protoForm(expr, doc = doc)
}

## removeMethod("initialize", "protoECall")
isECall <- function(obj){
    if(is.recursive(obj) && (typeof(obj) != "special"))
        identical(obj[[1L]], as.name("e"))
    else
        FALSE
}

protoForm <- function(expr, doc = character()){
    ## force evaluation of encapsulated expressions F(...), protoForm() and
    ## as.forms in order to generate subexpressions that are actually forms and
    ## not just expressions.
    pframe <- parent.frame()
    if(missing(expr)) expr <- expression()
    for(i in seq_along(expr)){
        sym <- expr[[i]]
        if(is.recursive(sym) && (sym[[1]] == as.name("protoForm") ||
                                 sym[[1]] == as.name("form") ||
                                 sym[[1]] == as.name("as.form"))){
            expr[[i]] <- eval(expr[[i]], pframe)
        }
    }
    new("protoForm", expr, doc = doc)
}

as.form <- function(from){
    new("protoForm", as.expression(from))
}

setAs("ANY", "protoForm",
      as.form)  ## todo: qualify as.pbm.sexp

setAs("expression", "protoForm",
      ## why  .makeNames here? new have to create them todo:
      function(from){
          names <- .makeNames(allNames(from))
          new("protoForm", from, names = names)
      })

setAs("call", "protoForm",
      function(from){
          new("protoForm", as.expression(from))
      })

##' Subset protoForm objects.
##'
##' @param x protoForm object
##' Return a protoForm object.
setMethod("[",
          signature(x = "protoForm"),
          function (x, i, j, ..., drop = TRUE){
              ## why [ strips class in the first place todo:
              as(callNextMethod(), "protoForm")
          })



###_ FORM WITH ENV
setClass("protoFormWithEnv",
         representation(environment = "environment", form_name = "character"),
         contains = "protoForm") ## used for printing, $ methods returns this object with environment slot set to .self

setMethod("show", signature(object = "protoFormWithEnv"),
          function(object) print.protoFormWithEnv(object, code = TRUE))

print.protoFormWithEnv <- function(x, code = TRUE,
                                   expand = getOption("protoClasses.print_expanded_forms"), ...){
    if(!is.null(expand) && expand)
        .print_protoFormWithEnv_expanded(x, code = code)
    else
        .print_ProtoFormWithEnv_colapsed(x, code = code)
}

## setMethod("print", signature(x = "protoFormWithEnv"),
## settle on S3 generic
.print_protoFormWithEnv_expanded <- function(x, code = TRUE, ...){
              .sub <- function(te){
                  for( i in seq_along(te)){
                      ## if( class(te)=="{")
                      ##     te[[1]] <- .sub(te[[1]])
                      if (is.recursive(te) && typeof(te) != "special"){
                          src <- getSrcref(te)
                          if(isECall(te[[i]])){
                              fname <- as.character(te[[i]][[2]])
                              if(length(expand <- get(fname, where)) &&
                                 is.null(accum[[fname]])){
                                  accum[[fname]] <- 1
                                  expand <- .sub(expand)[[1]] ## get rid of "expression"
                                  subst <- list(e = te[[i]],
                                                file = paste0(
                                                  "defined in ", .getType(.get_form_host(where, fname)),
                                                  ' (from ', utils::getSrcFilename(expand),
                                                  "#", utils::getSrcLocation(expand,"line")[[1]], ")"))
                                  ## if(class(expand) == "{")
                                  ##     tte <- c(expand[[1]], substitute(e(file), subst), expand[-1])
                                  ## else{
                                  tte <- substitute({e(file);expand}, subst)
                                  tte[[3]] <- expand ## preserve srcref
                                  te[[i]] <- tte
                              }
                          } else if (is.recursive(te[[i]]))
                              te[[i]] <- .sub(te[[i]])
                      }}
                  te
              }
              accum <- new.env()
              where <- x@environment
              ## first one is alwyas recursive expression
              cat(paste0("## ", x@form_name,  " defined in [",
                         .getType(.get_form_host(where, x@form_name)), "] (from ",
                         getSrcFilename(x[[1]]), "#", getSrcLocation(x[[1]])[[1]], ")\n"))
              out <- .sub(x@.Data)
              ## names(out) <- NULL
              out <- capture.output(print(out))
              cat(gsub("e[(](.*).\"(.*)\"", "## e(\\1 \\2", out), sep = "\n")
          }


.print_ProtoFormWithEnv_colapsed <- function(x, code = TRUE, ...){
    ## assumes that "name" exists and checks are not made.
    .print_local <- function(name, where, lev, code){
        if(exists(name, envir = where)){
            form <- get(name, envir = where)
            if(is(form, "protoForm")){
                cat(rep(".. ", lev),"e(", name, ") ", sep = "")
                if(length(form@doc)) {
                    cat("## DOC: \n")
                    print(form@doc)
                }
                for(i in seq_along(form)){
                    fm <- form[[i]]
                    if(isECall(fm)){
                        cat("\n")
                        Recall(as.character(fm[[2]]), where, lev = lev + 1L, code)
                    }else{
                        src <- utils::getSrcFilename(fm)
                        if(length(src))
                            cat('(from ', src,
                                "#", utils::getSrcLocation(fm,"line")[[1]], ")\n", sep = "")
                        else cat("\n")
                        if(code){
                            te <- as.expression(fm)[[1]]
                            ## expand <- .sub(fm)[[1]] ## get rid of "expression"
                            attr(te, "wholeSrcref") <- NULL ## kludge
                            attr(te, "srcfile") <- NULL ## kludge
                            cat(paste(paste(rep.int("   ", lev), collapse = ""),
                                      capture.output(print(te))),  sep ="\n")
                        }}
                }
            }else{
                cat(rep(".. ", lev),"e(", name, ") <-- missing ", sep = "")
            }
        }}
    ## cat("##", name, "\n", sep = "")
    f_names <- names(x)
    for(i in seq_along(x)){
        fm <- x[[i]]
        if(isECall(fm)){
            .print_local(as.character(fm[[2]]), x@environment, lev = 0L, code)
        }else{
            cat(f_names[[i]], "\n")
            te <- as.expression(fm)[[1]]
            attr(te, "wholeSrcref") <- NULL ## kludge
            attr(te, "srcfile") <- NULL ## kludge
            print(te)
        }
    }
    cat("\n")
}



###_ FORM DEFINITION
setClass("protoFormDefinition",
         representation(formClass = "character"),
         prototype(formClass = "protoForm")) # add host protoObject?

.installBinding_protoForm <- function(bindDefinition, container, bindName,
                                      after = NULL, returnBinding = bindName,
                                      reinstal = FALSE){
    ## * Used recursively:
    ## if bindDefinition is a form - assign in the whereEnv and return e(...)
    ## if bindDefinition is an expression - just return (as it is called recursively)
    ## * main idea: an assigned form should have only calls like e(aa.bb.cc) or
    ## simple expressions. No forms as children.
    ## * If the form's name is like aa.bb.cc ,check for existence of aa.bb and aa
    ## and install the eform e(aa.bb.cc) into aa.bb with the name "cc" and
    ## e(aa.bb) into "aa" with the name "bb".
    ## * If bindDefinition == NULL then this form ("aa.bb") and all the children forms
    ## ("aa.bb.cc.dd" etc) are removed from CONTAINER
    ## * if AFTER is nonnull it must be character or an integer specifying
    ## the position of the new form "cc" in the parent form "aa.bb"

    whereEnv <- as.environment(container@host)
    form <- bindDefinition

    ## bindName is aa.bb.cc
    rec_names <- unlist(strsplit(bindName, split = ".",  ## "aa" "bb" "cc"
                                 fixed = TRUE), use.names=FALSE)
    cum_names <- Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
                        c(paste(x[[1]], y, sep = "."), x), rec_names)
    if(is.null(form))
        return(.removeFormWithChildren(bindName, whereEnv))

    if(is(form, "protoForm")){
        ## create parent forms (i.e. aa.bb, aa) if needed:
        assigned <- TRUE
        last_assigned <- bindName
        while(assigned && length(cum_names) > 1L){
            assigned <-
                .assignSubForms(cum_names[[2]], ## aa.bb = form(cc = e(aa.bb.cc))
                                .makeEForm(cum_names[[1]]), whereEnv, after = after)
            last_assigned <- cum_names[[1L]]
            cum_names <- cum_names[-1L]
        }
        ## print(last_assigned)
        shortNames <- names(form) ## (dd, ff)
        firstNames <- gsub("\\..*", "", shortNames)
        longNames <- paste(bindName, shortNames, sep = ".") ## (aa.bb.cc.dd, aa.bb.cc.ff)
        newForm <-
            if(!reinstal && exists(bindName, envir = whereEnv[[".forms"]]))
                get(bindName, envir = whereEnv)
            else new("protoForm")
        stopifnot(is(newForm, "protoForm"))
        ## do not delete as yet:
        ## for(i in seq_along(firstNames)){
        ##     if(!is.null(newForm[[firstNames[[i]]]]))
        ##         stop("subexpression `", firstNames[[i]], "` in form `", bindName,
        ##              "` is already initialised. Use 'removeForm' first, or 'setForms' instead")
        ## }
        for(i in seq_along(firstNames)){
            newForm[[firstNames[[i]]]] <-
                ## if a form then install, if expression, just return
                .installBinding_protoForm(
                  form[[i]], container, longNames[[i]],
                  returnBinding = paste(bindName, firstNames[[i]], sep = "."))
        }
        ## -------------------------------------------------------------- #
        ## install newForm only if different                              #
        ## if(exists(bindName, envir = get(".forms", envir = whereEnv))){ #
        ##     oldForm <- get(bindName, envir = whereEnv)                 #
        ## if(!identical(newForm, oldForm)){                              #
        ## -------------------------------------------------------------- #
        ## todo: get the followining into unit test somehow
        if(exists(bindName, whereEnv) && 
           (ln <- length(setdiff(names(get(bindName, whereEnv)), names(newForm)))))                
            warning("warning: ", ln, " subforms removed in '", bindName, "' form")
        assign(bindName, newForm, envir = whereEnv)                                                
        .installBinding_default(new("protoFormDefinition", formClass = class(form)),
                                container, bindName, ".forms")
        .makeEexpr(returnBinding)
    }else{
        ## .assignForm should no be used directly to assign non protoForm objects !!
        form  ## returns as is (i.e. expression), nothing is assigned
      }
}

setMethod("installBinding", "protoForm", .installBinding_protoForm)

.makeEexpr <- function(formName)
    substitute(e(formName), list(formName = as.name(formName)))

.makeEForm <- function(formName){
    ## protoForm of the form form(cc = e(aa.bb.cc))
    short_name <- strsplit(formName, ".", fixed = TRUE)[[1L]]
    short_name <- short_name[[length(short_name)]]
    expr <- expression()
    expr[[short_name]] <- .makeEexpr(formName)
    new("protoForm",  expr)
}

.makeNames <- function(names){
    ## forms must have non-empty names (used in initialization for "protoForm")
    if(length(names)){
        if(is.null(names)||is.na(names))
            names <- rep.int("", length(names))
        if(!all(empty <- nzchar(names)))
            names[!empty] <- array(LETTERS, dim = sum(!empty))
        make.unique(names, sep = "")
    }else{
        character()
    }
}



###_ CONTAINER
setClass("formContainer",
         prototype = prototype(typeContainer = ".forms"),
         contains = "protoContainer")

setMethod("$", signature(x = "formContainer"),
          function(x, name){
              ## fixme: what should this do? now it return a string 
              get(name, envir = x)
          })

.dollarSet_formContainer <- function(x, name, value, error = TRUE, after = NULL){
    if(exists(name, envir = x) &&
       is(oldForm <- get(name, envir = x@host), "protoForm")){
        is_eCall <- unlist(sapply(oldForm, isECall))
        shortNames <- names(oldForm)
        longNames <- paste(name, shortNames, sep = ".")
        ##         lapply(seq_along(names(oldForm)[is_eCall]), function(nm){
        ##             ## shit!! FIXME:: TODO: does not work for complex forms
        ##             ## if use '!is(value[[nm]], "protoForm") && ' in check the protoForm
        ##             ## goes to instalation and installes subforms even if they are not present yet!!
        ##             if(!identical(oldForm[[nm]], value[[nm]]))
        ##                 stop("Can only modify non-form elements with \"setForms\" interface.
        ## Not true for element \"", longNames[[nm]], "\"
        ## Use \"initForms\" to install new forms.")
        ##         })
        value <- as(value, "protoForm")
        if(!identical(oldForm@.Data, value@.Data)){
            .removeFormWithChildren
            .installBinding_protoForm(value, x, name, reinstal = TRUE)
        }
        return(x)
    }else{
        if(error) stop("Object ", name,
                       " is not a valid form in the protoObject of type \"",
                       x@host$type, "\"\nUse `initForms` to initialize new forms")
        else substitute()
    }
}

.setForm <- function(x, name, value, after = NULL, error = TRUE){
    .dollarSet_formContainer(get(".forms", envir = x, inherits = F),
                             name, value, error, after)
}

setMethod("$<-", signature(x = "formContainer"),
          function(x, name) .dollarSet_formContainer(x, name, value))



###_ KILLERS
.removeFormWithChildren <- function(name, selfEnv){
    "removes recursively registred forms.
If name is aa.bb all the registered forms starting with aa.bb (inclusive) will
 be removed from selfEnv and selfEnv[['.forms']] container."
    contEnv <- get(".forms", envir = selfEnv)
    all_names <- .get_all_names(contEnv)
    to_remove <- grep(name, all_names, value = T, fixed = T)
    to_remove <- to_remove[substring(to_remove, 1, nchar(name)) == name]
    ## unlist is neede when to_remove is character(0)
    to_remove_container <- to_remove[unlist(sapply(to_remove, exists, envir = contEnv, inherit = FALSE))]
    to_remove_self <- to_remove[unlist(sapply(to_remove, exists, envir = selfEnv, inherit = FALSE))]
    remove(list = to_remove_container, envir = as.environment(contEnv))
    remove(list = to_remove_self, envir = as.environment(selfEnv))
    invisible(NULL)
}

.emptyFormWithChildren <- function(name, selfEnv){
    ## tothink: do we need this?
    "Emptyfy recursively registred forms.
If name is aa.bb, all the registered forms starting with aa.bb will
 be emptyfied (i.e. assigned F()) in selfEnv."
    contEnv <- get(".forms", envir = selfEnv)
    all_names <- .get_all_names(contEnv)
    ## FIXME: need only at the beggingin of the string.
    to_empty <- grep(name, all_names, value = T, fixed = T)
    to_empty <-
        to_empty[substring(to_empty, 1, nchar(name)) == name]
    ## unlist is needed in case to_empty is character(0)
    lapply(to_empty, assign,  value = F(), envir = selfEnv)
    invisible(NULL)
}

.summaryForms <- function(object){
    .forms <- get(".forms", envir = object)
    .infoContainer(.get_all_names(.forms), object, ".fields")
}

.assignSubForms <- function(formName, form, where, register = TRUE, after = NULL){
    " If oldForm with the name formName exists replace supbforms form FORM in oldForm,
assign form with formName otherwise.
 If AFTER is NULL just replace the subforms from old form by new subforms (the default)
 if <= 0 - preppend, if => length(oldForm) append. No name checks.
 Don't assign if new form is identical to the installed one."
    do_assign <- FALSE
    if(exists(formName, envir = get(".forms", envir = where))){
        ## adds forms to existing one
        oldForm <- get(formName, envir = where)
        if(!is.null(after)){
            if(is.character(after))
                after <- match(after, names(oldForm))
            else
                after <- as.integer(after)
            if(is.na(after))
                ## if not found by match or bigger number than length just append
                after <- NULL
        }
        newForm <-
            if(is.null(after)){
                newForm <- oldForm
                for(nm in allNames(form))
                    newForm[[nm]] <- form[[nm]] ## subforms should replace the old ones
                newForm
            }else if (after >= length(oldForm)){
                as(c(oldForm, form), class(oldForm))
            }else if (after <= 0){
                as(c(form, oldForm), class(oldForm))
            }else{
                as(c(oldForm[1L:after], form, oldForm[(after + 1L):length(oldForm)]), class(oldForm))
            }
        if(!identical(newForm, oldForm))
            do_assign <- TRUE
    }else{
        newForm <- form
        do_assign <- TRUE
    }
    if(do_assign){
        assign(formName, newForm, envir = where)
        .installBinding_default(new("protoFormDefinition", formClass = class(newForm)),
                                container = where[[".forms"]], formName)
    }
    do_assign
}

## .existsForm <- function(name, selfEnv){
##     exists(name, envir = selfEnv[[".forms"]])
## }

.getForm <- function(name, selfEnv)
    if(exists(name, envir = selfEnv[[".forms"]])){
        ## .dollarGet_formContainer(selfEnv[[".forms"]], name)
        new("protoFormWithEnv", get(name, envir = selfEnv),
            environment = selfEnv, form_name = name)
    }else{
        substitute()
    }


###_ INITIALIZE
setMethod("initialize", "protoForm",
          function(.Object, ...){
              .Object <- callNextMethod()
              ## forms always have names!!
              names(.Object) <- .makeNames(allNames(.Object))
              .Object
          })

.initForms <- function(forms, where, after = NULL, emptyforms = c()){
    "init the FORMS in the object WHERE"
    whereEnv <- as.environment(where)
    ## tothink: do I really need emptyforms?
    if(length(emptyforms) > 0L){
        forms <- c(forms, setNames(as.list(rep(expression(),
                                               length(emptyforms))),
                                   as.character(emptyforms)))
    }
    if(!is.null(after))
        forms <- rev(forms)
    ## non empty names?
    formNames <- names(forms)
    if(length(forms)>0 && ( is.null(formNames) ||
                           !all(nzchar(formNames))))
        stop("Arguments to 'initForms' must have nonempty names")
    if(!(all(which <- unlist(lapply(forms, function(x) is.language(x) || is.null(x))))))
        stop("Arguments to 'initForms' must be a subclass of name, call or expression /see ?is.language/. Not true for ", paste(formNames[!which], collapse = ", "))
    ## look for objects to remove (new definition is NULL)
    removeThese <- sapply(forms, is.null)
    if(any(removeThese)){
        remFormNames <- formNames[removeThese]
        lapply(remFormNames, .removeFormWithChildren, selfEnv = whereEnv)
    }
    ## convert all to protoForm objects
    forms <- lapply(forms, function(el) as(el, "protoForm"))
    ## install all bindings and update the container
    formNames <- names(forms)
    for(i in seq_along(forms))
        installBinding(forms[[i]], whereEnv[[".forms"]], formNames[[i]], after = after)
    invisible(formNames)
}
