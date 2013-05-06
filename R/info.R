
.infoContainer <- function(names, pObject, container){
    tcollate <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", tcollate))
    Sys.setlocale("LC_COLLATE", "C")
    containerEnv <-
        if(is.environment(container))
            container
        else
            as.environment(get(container, envir = pObject))
    all_names <- c()
    out <- data.frame(row.names = names)
    while(!(identical(containerEnv, emptyenv()) || is.null(pObject))){
        names_in <- names %in% ls(containerEnv, all.names = TRUE)
        type <- get(".type", envir = pObject)
        out <- cbind(names_in, out)
        names(out)[[1]] <- type
        containerEnv <- parent.env(containerEnv)
        pObject <- get(".prototype", envir = pObject)
    }
    iout <- as.matrix(out)
    out[iout] <- " +"
    out[!iout] <- "  "
    out <- out[order(row.names(out)),, drop = FALSE]
    predots <- unlist(lapply(strsplit(row.names(out), ".", fixed = T),
                             function(el) paste(rep(". ", length(el) - 1), collapse = "")))
    row.names(out) <- paste(predots, row.names(out), sep = "")
    out
}

.infoCell <- function(object){
    if(!is(object, "envProtoClass")) stop("Not a valid object from class envProtoClass")
    info_local <- function(x, info_chain){
        x <- as.environment(x)
        info <- c(cellType = x[[".type"]],
                  homeCxtType = .getType(x[[".self"]][[".homeContext"]]),
                  class = class(x[[".self"]]),
                  homeCxtClass = class(x[[".self"]][[".homeContext"]]))
        if(isRoot(x)) rbind(info, info_chain)
        else Recall(x[[".prototype"]], rbind(info, info_chain))
    }
    out <- info_local(object, data.frame(cellType = "--",
                                         homeCxtType = "---",
                                         class = "--",
                                         homeCxtClass = "--"))
    out[-nrow(out), ]
}

.infoContext <- function(object){
    if(!is(object, "protoContext")) stop("Not a valid object from class protoContext")
    info_local <- function(x, info_chain){
        x <- as.environment(x)
        info <- c(contextType = x[[".type"]],
                  class = class(x[[".self"]]))
        if(isRoot(x)) rbind(info, info_chain)
        else Recall(x[[".prototype"]], rbind(info, info_chain))
    }
    out <- info_local(object, data.frame(contextType = "--",
                                         class = "--"))
    out[-nrow(out), ]
}


infoForms <- function(pObject, names = NULL, regexp = NULL, all.names = FALSE,
                      expandCode = FALSE) {
    ## breaks down the changes tot he forms in the proto - history
    names <-
        if(!is.null(regexp)){
            ## all.names ignrored
            names <- .get_all_names(get(".forms", envir = pObject))
            grep(regexp, names, value = TRUE)
        }else if(all.names){
            ## names ignored
            .get_all_names(get(".forms", envir = pObject))
        }else if(is.null(names)){
            names <- .get_all_names(get(".forms", envir = pObject))
            names[names %in% ls(pObject, all.names =  TRUE)]
        }
    res <- replicate(length(names),
                     data.frame(check.names = FALSE), simplify = FALSE)
    names(res) <- names
    while(!is.null(pObject)){
        type <- get(".type", envir = pObject)
        proto <- get(".prototype", envir = pObject)
        for(nm in names){
                                        #            eval(substitute(
            ## blanks if not "nm" is not defined in pObject
            res[[nm]][[type]] <- rep(list(" "), NROW(res[[nm]]))
            if(exists(nm, envir = pObject, inherits = FALSE)){
                obj <- get(nm, envir = pObject, inherits = FALSE)
                if(!is(obj, "protoForm"))
                    warning("Object \"", nm, "\" in the pObject \"", type, "\", is not a protoForm.")
                names_obj <- names(obj)
                ## compare with the inherited object
                if(is.null(proto) ||
                   !exists(nm, envir = get(".forms", envir = proto))){
                    proto_obj <- list()
                }else{
                    proto_obj <- get(nm, envir = proto)
                }
                ## Names in obj:
                for(nm_el in names_obj){ # as.character needed here, if sapply returns list()
                    res[[nm]][[nm_el, type]] <-
                        if(length(obj) == 0)
                            structure("0", code = deparse(obj[[nm_el]]))
                        else if(is.null(proto_obj[[nm_el]]))
                            ## added
                            structure("+", code = deparse(obj[[nm_el]]))
                        else if(identical(proto_obj[[nm_el]], obj[[nm_el]]))
                            ## same
                            structure("=", code = deparse(obj[[nm_el]]))
                        else
                            ## different
                            structure("/", code = deparse(obj[[nm_el]]))
                }
                ## Names deleted:
                if(length(removed_names <-
                          names(proto_obj)[!names(proto_obj) %in% names_obj]))
                    res[[nm]][removed_names, type] <- rep.int("-", length(removed_names))
                ## doc and code for nm in type object
                attr(res[[nm]][[type]], "doc") <- obj@doc
                attr(res[[nm]][[type]], "code") <- deparse(obj)
            }
        }
        pObject <- proto
    }
    res <- mapply(function(el, nm){
        el[is.na(el)] <- " "
        if(nrow(el) == 0L){
            el[1, ] <- "0"## fixme:  expression() will give an 0 - row df here
        }
        el[" "] <- "|"
        el$form <- paste(formatC(seq.int(NROW(el)), digits = 2), row.names(el))
        el <- rbind(rep.int(" ", length(names(el))), el)
        el$form[[1]] <- paste("", nm, collapse = "")
        row.names(el) <- NULL
        el[rev(names(el))]
    }, res, names(res), SIMPLIFY = FALSE)
    class(res) <- c("infoForms", class(res))
    res
}

as.data.frame.infoForms <- function(x){
    for(nm in names(x)){
        x[[nm]]$`  ` <- "|"
        x[[nm]]$`  `[[1]] <- " "
        x[[nm]]$code <-
            apply(x[[nm]], 1, function(row){
                code <- NULL
                col_nr <- length(row)
                while(is.null(code) && col_nr > 2){
                    code <- attr(row[[col_nr]], "code")
                    col_nr <- col_nr - 1L
                }
                code <- paste(code, collapse = " ")
                if(nchar(code) < 30)
                    code
                else if (length(grep("^e\\(", code)))
                    "e(...)"
                else
                    "< Expression >"
            })
        ## if(is.na(res[[nm]][nm_el, "<code>"])){
    }
    res <- do.call(rbind, unname(x))
    res$form <- format(res$form, justify = "left")
    res
}

print.infoForms <- function(x, ...){
    print.data.frame(as.data.frame.infoForms(x))
}

## templateFile <- "template.html" ## system.file("brew", "default", "results.brew.html",
## ##             package = "sos")
## template <- file(templateFile, encoding = "utf-8", open = "r")
## xenv <- new.env()
## assign("form", "my form", envir = xenv)
## assign("x", x, envir = xenv)
## assign("title", "my title", envir = xenv)
## brew(template, File, envir = xenv)
## close(template)
## browseURL(File)

## browseDefinition <-
##     function (x, File, title = "Definition Browser", header1 = "", header2 = "",
##               openBrowser = TRUE, stylesheet = "info.dark", ...)
## {
##     ## x should be a df.
##     library(brew)
##     library(highlight)
##     if (nrow(x) < 1) {
##         cat("x has zero rows;  nothing to display.\n")
##         if (missing(where))
##             where <- ""
##         return(invisible(where))
##     }
##     if (missing(File))
##         f0 <- tempfile()
##     for (i in 1:111) {
##         File <- paste(f0, ".html", sep = "")
##         fInf <- file.info(File)
##         if (all(is.na(fInf)))
##             break
##         f0 <- paste(f0, "1", sep = "")
##     }
##     Dir <- dirname(File)
##     if (Dir == ".") {
##         Dir <- getwd()
##         File <- file.path(Dir, File)
##     }else{
##         dc0 <- dir.create(Dir, FALSE, TRUE)
##     }
##     js <- file_path_as_absolute("~/works/R_dev/pbm/info.js")
##     css <-
##         switch(stylesheet,
##                info.dark = file_path_as_absolute("~/works/R_dev/pbm/info.dark.css"),
##                file_path_as_absolute("~/works/R_dev/pbm/info.css")
##                )
##     if (!file.exists(js)) {
##         stop("Unable to locate '", js, "' file")
##     }else if(!file.exists(css)){
##         stop("Unable to locate '", css, "' file")
##     }else {
##         file.copy(js, Dir, overwrite = TRUE)
##         file.copy(css, Dir, overwrite = TRUE)
##     }
##     templateFile <- "~/works/R_dev/pbm/template.html"
##     if (!file.exists(templateFile))
##         stop("Unable to locate '~/works/R_dev/pbm/template.html' file")
##     template <- file(templateFile, encoding = "utf-8", open = "r")
##     xenv <- new.env()
##     assign("header1", header1, envir = xenv)
##     assign("header2", header2, envir = xenv)
##     assign("x", x, envir = xenv)
##     assign("title", title, envir = xenv)
##     assign("stylesheet", basename(css))
##     brew(template, File, envir = xenv)
##     close(template)
##     FileDefinition <- file.info(File)
##     if (is.na(FileDefinition$size) || FileDefinition$size <= 0) {
##         if (is.na(FileDefinition$size)) {
##             stop("Brew did not create file ", File)
##         }
##         else {
##             stop("Brew created a file of size 0")
##         }
##     }else if (openBrowser) {
##         if (is.na(FileDefinition$size)) {
##             warning("Did not create file ", File, ";  nothing to give to a browser.")
##         }
##         else {
##             if (FileDefinition$size <= 0) {
##                 warning("0 bytes in file ", File, ";  nothing to give to a browser.")
##             }
##             else {
##                 browseURL(File)
##             }
##         }
##     }
##     invisible(File)
## }

## browseForms <-
##     function(pObject, names = NULL, regexp = NULL, all.names = FALSE, stylesheet = "info.dark", ...) {
##         x <- as.data.frame(.infoForms(pObject = pObject, names = names, regexp = regexp,
##                                       all.names = all.names))
##         title <- gettextf("Form info for %s of type \" %s\"",
##                           class(pObject), .getType(pObject))
##         header1 <- title
##         header2 <- paste("Definition for the following names: \n",
##                          paste(names, collapse = ", "))
##         browseDefinition(x, title = title, header1 = header1, header2 = header2, stylesheet = source, ...)
##     }
