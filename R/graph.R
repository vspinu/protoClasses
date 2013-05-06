## Rgraphvis functionality
## setAs("cellContainer", "graphNEL", .as_cellContainer_graphNEL)
## setAs("protoContext", "graphNEL", .as_cellContainer_graphNEL)

setAs("cellContainer", "graphNEL",
      function(from){
          require(graph)
          env_from <- as.environment(from)
          all_names <- ls(env_from, all.names = TRUE)
          leaf_names <- leafNames(from)
          .gr <- new("graphNEL", nodes=all_names, edgemode = "directed")
          ## NODES
          nodeDataDefaults(.gr, "short_name") <- "?"
          nodeDataDefaults(.gr, "type") <- "?" # "leaf" or "prototype"
          nodeData(.gr, all_names, "short_name") <-
              sapply(all_names, function(nm) .getType(env_from[[nm]], fullName = F))
          nodeData(.gr, all_names, "type") <- "prototype"
          nodeData(.gr, leaf_names, "type") <- "leaf"
          ## EDGES
          edgeDataDefaults(.gr, "type") <- "?" # "model" or "prototype"
          ## add edges: based only on leaf_names!
          lapply(leaf_names, function(nm){
              rec_names <- unlist(strsplit(nm, split = ".", fixed = TRUE))     ## "aa" "bb" "cc"
              cum_names <-
                  Reduce(function(x, y) ## "aa.bb.cc" "aa.bb"    "aa"
                         c(paste(y, x[[1]], sep = "."), x), rev(rec_names), right = FALSE)
              suppressWarnings({
                  .gr <<- addEdge(cum_names[ -1], cum_names[-length(cum_names)], .gr)
                  edgeData(.gr, cum_names[ -1], cum_names[-length(cum_names)], "type") <<- "prototype"
              })
          })
          .gr
      })

## ## gR <- as(M[[".cells"]], "graphNEL")
## ## str(renderGraph(layoutGraph(gR)))
## ## plot(gR)
## ## edges(gR)
## setMethod("plot", c("protoContainer", "ANY"),
plot.protoContainer <- function(x, y, col.prototype="cyan",
                   col.leafs="yellow",
                   layoutType = c("dot", "neato", "twopi", "circo", "fdp"),
                   plot.root=F, types = NULL, 
                   fill = "Set2", #if character of length 1, it is a brewer palette name
                   shape = c("circle", "ellipse", "box"),
                   col = c("black", "darkgreen", "darkblue"),
                   cex = 1,
                   lwd = 2,
                   textCol = "black",
                   bg = "gray20",
                   ...){
              library(Rgraphvis)
              x <- as(x, "graphNEL")
              ## old_par <- par(bg="gray20")
              if(!plot.root){
                  x <- subGraph(nodes(x)[!nodes(x)%in%"*"], x)
              }
              .local_type_props <- function(prop, nr, type, names)
                  setNames(rep(prop, length.out = nr)[type], names)
              ## rearange common labels such that same type nodes and edges have
              ## same graphical properties:
              typeNodes <- sapply(nodeData(x), function(nd) nd$type)
              if(length(types) != 0L){
                  x <- subGraph(names(typeNodes[typeNodes %in% types]), x)
                  typeNodes <- sapply(nodeData(x), function(nd) nd$type)
              }
              typeEdges <- sapply(edgeData(x), function(nd) nd$type)
              common <- intersect(typeNodes, typeEdges)
              all_labels <- unique(c(typeEdges, typeNodes))
              all_labels <- c(common, setdiff(all_labels, common))
              typeNodes <- factor(typeNodes, levels = all_labels)
              typeEdges <- factor(typeEdges, levels = all_labels)
              ## NODES
              labels <- sapply(nodeData(x), function(nd) nd$short_name)
              nodeNames <- names(typeNodes)
              if(is.character(fill) && length(fill) == 1L){
                  library(RColorBrewer)
                  fill <- brewer.pal(max(length(levels(typeNodes)), 3L), fill)
              }
              nrt <- length(levels(typeNodes))
              ## EDGES
              edgeNames <- names(typeEdges)
              names(typeEdges) <- edgeNames <- gsub("\\|", "~", edgeNames)
              ## if(is.character(fill) && length(fill) == 1L)
              ##     fill <- brewer(max(length(levels(type)), 3L), fill)
              ## LAYOUT
              layoutType <- layoutType[[1]]
              x <- layoutGraph(x, layoutType=layoutType)
              nodeRenderInfo(x) <-
                  list(lwd = .local_type_props(lwd, nrt, typeNodes, nodeNames),
                       cex = .local_type_props(cex, nrt, typeNodes, nodeNames),
                       textCol = .local_type_props(textCol, nrt, typeNodes, nodeNames),
                       fill = .local_type_props(fill, nrt, typeNodes, nodeNames),
                       textColor = .local_type_props(fill, nrt, typeNodes, nodeNames),
                       shape = .local_type_props(shape, nrt, typeNodes, nodeNames),
                       label = labels)
              edgeRenderInfo(x) <-
                  list(col = .local_type_props(fill, nrt, typeEdges, edgeNames))
              renderGraph(x)
              ## par(old_par)
          }

## setMethod("plot", c("protoContext", "missing"),
##           def= plotCellGraph )
## setMethod("plot", c("protoContainer", "ANY"),
##           def = plotCellGraph)
