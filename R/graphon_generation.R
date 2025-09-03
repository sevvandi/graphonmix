#' Generates a sparse graph of star graphs
#'
#' Generates a union of star graphs given the weights
#'
#' @param wts The proportion of the hub degrees
#' @param n The number of nodes in the new graph
#'
#' @return A disjoint union of star graphs
#'
#' @examples
#' library(igraph)
#' wts <- c(0.5, 0.3, 0.2)
#' gr <- generate_star_union(wts, n = 100)
#' gr
#'
#' @export
generate_star_union <- function(wts, n){
  # wts is the mass partition
  stopifnot("Weights should add up to 1" = sum(wts) == 1)
  n = as.integer(n)

  star_sizes <- ceiling(n*wts)
  for(i in 1:length(star_sizes)){
    gr <- igraph::make_star(star_sizes[i], mode= "undirected")
    if(i == 1){
      gr2 <- gr
    }else{
      gr2 <- gr2 %du% gr
    }
  }
  gr2
}



graph_join <- function(gr1, gr2, p = 0.5, opt = 2) {
  # p is the proportion of nodes in the smaller graph to be joined
  # Check that inputs are igraph objects
  if (!igraph::is.igraph(gr1) || !igraph::is.igraph(gr2)) {
    stop("Both inputs must be igraph objects")
  }

  # Get the number of vertices in each graph
  n1 <- igraph::vcount(gr1)
  n2 <- igraph::vcount(gr2)

  # Create a disjoint union of the two graphs
  joined_graph <- igraph::disjoint_union(gr1, gr2)

  minn <- min(n1, n2)*p
  if(opt == 2){
    # The other option - not the disjoint union
    edgecount <- ceiling(ecount(gr1)*p)
    node1_sample <- sample(1:(n1+n2), edgecount, replace = TRUE)
    node2_sample <- sample(1:(n1+n2), edgecount, replace = TRUE)
    new_edges <- cbind(node1_sample, node2_sample)
    # Add the new edges to the joined graph
    joined_graph <- igraph::add_edges(joined_graph, as.vector(t(as.matrix(new_edges))))
    joined_graph <- igraph::simplify(joined_graph)
  }

  # Preserve vertex names if they exist
  if (!is.null(igraph::V(gr1)$name) && !is.null(igraph::V(gr2)$name)) {
    igraph::V(joined_graph)$name <- c(igraph::V(gr1)$name, igraph::V(gr2)$name)
  }

  # Return the joined graph
  return(joined_graph)
}


#' Generate a (U,W) mixture graph
#'
#' Generate a (U,W) mixture graph from a dense graphon W and a mass partition
#' corresponding to a line graph graphon U.
#'
#' @param  W The dense graphon. This is a symmetric matrix with values in [0,1]
#' @param wts The degree proportions of the hub degrees. Need to add up to 1. This
#' is the mass partition corresponding to the line graph graphon U.
#' @param nd The number of nodes in the dense part of the graph
#' @param ns The number of nodes in the sparse part of the graph
#' @param p The nodes to be added as a proportion of the edges in the dense part
#' @param option Graph joining option. If \code{option == 1} then a disjoit union is
#' considered. If \code{option == 2} the two graphs are joined randomly with the
#' number of edges specified by \code{p}.
#'
#' @return A graph sampled from the (U,W) mixture.
#'
#' @examples
#' library(igraph)
#' W <- matrix(0.1, nrow = 100, ncol = 100)
#' wts <- c(0.5, 0.3, 0.2)
#' ns <- 200
#' nd <- 100
#' p <- 0.5
#' gr <- sample_mixed_graph(W, wts, nd, ns, p, option = 2)
#' gr
#' @export
sample_mixed_graph <- function(W, wts, nd, ns, p, option){
  # W is a graphon
  stopifnot("W is not a matrix " = is.matrix(W))
  stopifnot("W is not symmetric " = isSymmetric(W))
  stopifnot("wts need to add to 1 " = sum(wts) == 1)
  stopifnot("p needs to be between 0 and 1 " = (p >=0) & (p <= 1))
  stopifnot("Option needs to be either 1 or 2 " = option %in% c(1,2))

  nd <- as.integer(nd)
  ns <- as.integer(ns)
  grdense <- sample_graphon(W, nd)
  grsparse <- generate_star_union(wts, ns)
  gr <- graph_join(grdense, grsparse, p=p,  opt = option)
  gr
}



#' Generates a graph given a graphon
#'
#' Generates a graph given a dense graphon W.
#'
#' @param W A graphon given by a matrix
#' @param n The number of nodes of the sampled graph
#'
#' @return A graph sampled from the graphon W with n nodes
#'
#' @examples
#' library(igraph)
#' W <- matrix(0.2, nrow = 100, ncol = 100)
#' gr <- sample_graphon(W, n= 100)
#'
#' @export
sample_graphon <- function(W, n){
  graphonscaled <- scale_graphon(W, n)
  adj <- generate_adj_from_probs(graphonscaled)
  gr <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  gr
}


#' Computes empirical graphon from graph
#'
#' Computes empirical graphon given a graph
#'
#' @param gr A graph
#' @param n Dimension of the graphon matrix
#'
#' @return The empirical graphon
#'
#' @examples
#' library(igraph)
#' gr <- sample_gnp(1000, p=0.2)
#' emp <-  empirical_graphon(gr, n = 100)
#'
#' @export
empirical_graphon <- function(gr, n=NULL){
  adj <- igraph::get.adjacency(gr)
  adj2 <- arrange_matrix(adj)
  if(!is.null(n)){
    adj2 <- scale_graphon(adj2, n)
  }
  adj2
}


generate_adj_from_probs <- function(probsmat) {
  probs <- probsmat[upper.tri(probsmat, diag=FALSE)]
  vals <- stats::rbinom(n = length(probs), size = 1, prob = probs)
  adj <- matrix(0, nrow = nrow(probsmat), ncol = nrow(probsmat))
  adj[upper.tri(adj, diag=FALSE)] <- vals
  adj <- adj + t(adj)
  diag(adj) <- 0
  adj
}


#' Scales a graphon to an nxn matrix
#'
#' Scales a graphon to an nxn matrix suitable for large adjacency matrices
#'
#' @param W A graphon given as a symmetric square matrix
#' @param n The dimension of the output matrix
#'
#' @return Scaled nxn graphon
#'
#' @examples
#' library(igraph)
#' gr <- sample_gnp(1000, p=0.2)
#' adj <- as_adjacency_matrix(gr)
#' W <- scale_graphon(adj, 100)
#'
#' @export
scale_graphon <- function(W, n){
  # W is a n x n matrix
  # scalar is a scalar
  # n is the size of the output matrix
  W <- as.matrix(W)
  img <- imager::as.cimg(W)
  Wscaled <- as.matrix(imager::resize(img, n, n, interpolation_type = 2))
  Wscaled
}
