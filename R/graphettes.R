#' Samples a graph from a sparsified graphon
#'
#' Samples a graph from a sparsified graphon given by a_n W, where W is the graphon and a_n is generally a sequence converging to zero.
#'
#' @param W The graphon. This is a symmetric matrix with values in [0,1]
#' @param rho_n A function. Default is 1.
#' @param n The number of nodes in the graph to sample
#'
#' @return gr A graph sampled from rho_n W.
#'
#' @examples
#' library(igraph)
#' f1 <- function(n) 10/n
#' W <- matrix(0.2, nrow = 100, ncol = 100)
#' gr <- sample_sparse_graphon(W, f1, n = 100)
#' gr
#'
#' @export
sample_sparse_graphon <- function(W, rho_n = NULL, n){

  # rho_n is a function

  # check if W is a symmatrix matrix
  if (!is.matrix(W)) {
    stop("Input W is not a matrix.")
  }

  if (nrow(W) != ncol(W)) {
    stop("W is not square.")
  }

  if (!isSymmetric(W)) {
    stop("W is not symmetric.")
  }

  if( (max(W) >= 1) |(min(W) <= 0)){
    stop("W contains values outside of [0,1].")
  }

  if(is.null(rho_n)){
    rho_n  <- function(n) 1
  }else if(rho_n(10) < rho_n(100)){
    stop("Function rho_n cannot be increasing!")
  }

  Wn <- rho_n(n)*W
  gr <- sample_graphon(Wn, n)
  gr
}



#' Samples a graph from a graphette
#'
#' Samples a graph from a graphette given by (W, a_n, f), where W is the graphon, a_n is generally a sequence converging to zero and f is a graph edit funcion.
#'
#' @param W The graphon. This is a symmetric matrix with values in [0,1]
#' @param rho_n A function. Default is 1.
#' @param graph_edit_f The graph edit function. For star functions it can be
#' one of \code{'star_f1'}, \code{'star_f2'}, \code{'star_f3'}, \code{'star_f4'} or \code{'star_f5'}. To add rings, it needs to be \code{'add_rings'}. The function \code{'remove_cycles'} removes cycles.
#' @param n The number of nodes in the graph to sample
#' @param t_or_p The parameter for star function or the ring function. For the
#' ring function it needs to be between 0 and 1.
#' @param ring_sizes The size of rings to add if the graph edit function is the ring function. default values set to \code{c(5,6)}.
#'
#' @return gr A graph sampled from rho_n W.
#'
#' @examples
#' library(igraph)
#' # Example 1
#' f1 <- function(n) 10/n
#' W <- matrix(0.2, nrow = 100, ncol = 100)
#' gr <- sample_graphette(W, rho_n = f1, graph_edit_f = 'add_rings', n = 100, t_or_p = 0.5)
#' gr
#'
#' # Example 2
#' gr <- sample_graphette(W, rho_n = f1, graph_edit_f = 'star_f1', n = 100, t_or_p = 3)
#' gr
#' @export
sample_graphette <- function(W,
                             rho_n = NULL,
                             graph_edit_f = NULL,
                             n = 100,
                             t_or_p = 0.5,
                             ring_sizes = c(5,6)){

  gr1 <- sample_sparse_graphon(W, rho_n, n)

  star_func_list <- list('star_f1', 'star_f2', 'star_f3', 'star_f4', 'star_f5')

  if(is.null(graph_edit_f)){
    gr <- gr1
  }else if(graph_edit_f %in% star_func_list){
    gr <- add_star_edges(gr1, match.fun(graph_edit_f), t_or_p)
  }else if (graph_edit_f == 'add_rings'){
    if(t_or_p > 1 | t_or_p < 0){
      stop('Parameter t_or_p need to be between 0 and 1 for rings!')
    }
    gr <- add_rings(gr1, proportion = t_or_p, ring_sizes = ring_sizes)
  }else if (graph_edit_f =='remove_cycles'){
    gr <- remove_cycles(gr1)
  }

  gr
}


add_star_edges <- function(gr, star_f, t) {
  # This function adds star edges to a graph based on a star graph function.
  #
  # Args:
  #   gr: An igraph graph object.
  #   star_f: A function representing the starness of nodes in a graph.
  #   t: Time
  #
  # Returns:
  #   A modified igraph graph object with added star edges.

  num_nodes <- vcount(gr)
  star_samples <- sample_star_function(star_f, num_nodes, t)

  star_inds <- which(star_samples > 0)
  for (i in star_inds) {
    num_star_edges <- star_samples[i]
    # add num_star_edges vertices to gr
    new_vertices <- seq(num_nodes + 1, num_nodes + num_star_edges)
    gr <- igraph::add_vertices(gr, nv = num_star_edges)
    # connect vertex i to new vertices
    gr <- igraph::add_edges(gr, c(rbind(rep(i, num_star_edges), new_vertices)))
    num_nodes <- igraph::vcount(gr)
  }

  return(gr)
}


#' Adds stars to nodes
#'
#' @param xvals Values between 0 and 1 from the graphon
#' @param t A parameter indicating the strength of hubs
#'
#' @return a vector of values
#'
#' @examples
#' # example code
#' st <- star_f1(runif(10), 3)
#' st
#' @export
star_f1 <- function(xvals, t){
  vals <- exp(-1*(xvals - 0.2)*t)
  return(vals)
}

#' Adds stars to nodes
#'
#'@inheritParams star_f1
#' @return a vector of values
#'
#' @examples
#' # example code
#' st <- star_f2(runif(10), 3)
#' st
#' @export
star_f2 <- function(xvals, t){
  vals <- t*exp(-1*(xvals - 0.2))
  return(vals)
}

#' Adds stars to nodes
#'
#'@inheritParams star_f1
#' @return a vector of values
#'
#' @examples
#' # example code
#' st <- star_f3(runif(10), 3)
#' st
#' @export
star_f3 <- function(xvals, t){
  vals <- t*exp(-1*(xvals + 1))
  return(vals)
}

#' Adds stars to nodes
#'
#'@inheritParams star_f1
#' @return a vector of values
#'
#' @examples
#' # example code
#' st <- star_f4(runif(10), 3)
#' st
#' @export
star_f4 <- function(xvals, t){
  k=3
  vals <- rep(k*t, length(xvals))
  return(vals)
}

#' Adds stars to nodes
#'
#'@inheritParams star_f1
#' @return a vector of values
#'
#' @examples
#' # example code
#' st <- star_f5(runif(10), 3)
#' st
#' @export
star_f5 <- function(xvals, t){
  vals <- t/10*exp(-1*(xvals - 0.2))
  return(vals)
}

# Star functions and edge addition for graphs
sample_star_function <- function(star_f, num_samples, t) {
  # This function samples from a star graph function.
  #
  # Args:
  #   star_f: A function representing the starness of nodes in a graph.
  #   num_samples: Number of samples to draw.
  #   t: Time
  #
  # Returns:
  #   A vector of sampled values from the star graph function.

  xvals <- seq(0, 1, length.out = num_samples)
  star_vals <- star_f(xvals = xvals, t = t)
  samples <- stats::rpois(num_samples, lambda = star_vals)

  return(samples)
}



add_rings <- function(gr, proportion = 0.1, ring_sizes = c(5, 6)) {
  # Input validation
  if (!igraph::is.igraph(gr)) {
    stop("Input must be an igraph object")
  }
  if (proportion <= 0 || proportion > 1) {
    stop("Proportion must be between 0 and 1")
  }

  # Calculate number of rings to add
  n_vertices <- vcount(gr)
  n_rings <- max(1, round(n_vertices * proportion))

  # Start with the original graph
  result_graph <- gr

  # Add rings
  for (i in 1:n_rings) {
    # Randomly choose ring size (5 or 6 vertices)
    ring_size <- sample(ring_sizes, 1)

    # Create a ring
    ring <- igraph::make_ring(ring_size)

    # Get current number of vertices before adding ring
    current_n <- igraph::vcount(result_graph)

    # Combine the graphs (disjoint union)
    result_graph <- igraph::disjoint_union(result_graph, ring)

    # Select a random node from the original graph to connect to
    selected_node <- sample(1:current_n, 1)

    # Select a random node from the newly added ring to connect from
    # The ring vertices are now indexed from (current_n + 1) to (current_n + ring_size)
    ring_node <- sample((current_n + 1):(current_n + ring_size), 1)

    # Add an edge connecting the selected node to the ring
    result_graph <- igraph::add_edges(result_graph, c(selected_node, ring_node))
  }

  return(result_graph)
}


remove_cycles <- function(gr) {
  # Convert to a directed acyclic graph by removing back edges
  # Use a spanning tree approach
  gr <- igraph::as.undirected(gr)
  spanning_tree <- igraph::mst(gr)
  return(spanning_tree)
}
