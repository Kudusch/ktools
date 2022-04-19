#' A better summary function for igraph graphs
#'
#' @description A summary-function for igraph objects.
#' @param object An object to summarise.
#' @param ... Additional arguments affecting the summary produced.
#' @examples
#'\dontrun{
#' summary(g)
#' }
#' @export summary.igraph
#' @export
summary.igraph <- function(object, ...) {
    n.nodes <- length(igraph::V(object))
    n.edges <- length(igraph::E(object))
    if (igraph::is.connected(object)) {
        connected <- "connected"
    } else {
        connected <- "disconnected"
    }
    if (igraph::is.directed(object)) {
        directed <- "directed"
    } else {
        directed <- "undirected"
    }

    edge.attr <- igraph::edge_attr(object)
    edge.attr.names <- names(edge.attr)
    edge.attr.types <- unlist(lapply(edge.attr.names, function(x) {typeof(edge.attr[[x]])}))

    vert.attr <- igraph::vertex_attr(object)
    vert.attr.names <- names(vert.attr)
    vert.attr.types <- unlist(lapply(vert.attr.names, function(x) {typeof(vert.attr[[x]])}))

    message("An igraph object (", connected, "/", directed, ") with")
    message("  ", n.nodes, " vertices")
    message("  ", n.edges, " edges")
    message("\nEdge Attributes:")
    for (i in 1:length(edge.attr)) {
        message("  ", i, ": ", edge.attr.names[i], " (", edge.attr.types[i], ")")
    }
    message("\nVertex Attributes:")
    for (i in 1:length(vert.attr)) {
        message("  ", i, ": ", vert.attr.names[i], " (", vert.attr.types[i], ")")
    }
}
