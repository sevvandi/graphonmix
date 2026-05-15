#' #' Samples a graph from a sparsified graphon
#' #'
#' #' Samples a graph from a sparsified graphon given by a_n W, where W is the graphon and a_n is generally a sequence converging to zero.
#' #'
#' #' @param W The graphon. This is a symmetric matrix with values in [0,1]
#' #' @param rho_n A function. Default is 1.
#' #' @param n The number of nodes in the graph to sample
#' #'
#' #' @return gr A graph sampled from rho_n W.
#' #'
#' #' @examples
#' #' library(igraph)
#' #' f1 <- function(n) 10/n
#' #' W <- matrix(0.2, nrow = 100, ncol = 100)
#' #' gr <- sample_sparse_graphon(W, f1, n = 100)
#' #' gr
#' #'
#' #' @export
#' sample_sparse_graphon <- function(W, rho_n = NULL, n){
#'
#'   # rho_n is a function
#'
#'   # check if W is a symmatrix matrix
#'   if (!is.matrix(W)) {
#'     stop("Input W is not a matrix.")
#'   }
#'
#'   if (nrow(W) != ncol(W)) {
#'     stop("W is not square.")
#'   }
#'
#'   if (!isSymmetric(W)) {
#'     stop("W is not symmetric.")
#'   }
#'
#'   if( (max(W) >= 1) |(min(W) <= 0)){
#'     stop("W contains values outside of [0,1].")
#'   }
#'
#'   if(is.null(rho_n)){
#'     rho_n  <- function(n) 1
#'   }else if(rho_n(10) < rho_n(100)){
#'     stop("Function rho_n cannot be increasing!")
#'   }
#'
#'   Wn <- rho_n(n)*W
#'   gr <- sample_graphon(Wn, n)
#'   gr
#' }
#'
#'
#'
#'
#'
#'
#' # check_fn <- function(fn) {
#' #   fn_name <- deparse(substitute(fn))
#' #   allowed <- c("rho_n", "rho_n2", "rho_n3")
#' #
#' #   if (!fn_name %in% allowed) {
#' #     stop(paste("fn must be one of:", paste(allowed, collapse = ", ")))
#' #   }
#' # }
#'
#'
#' ### HERE!!!!
#'
#' sample_graphette <- function(W, rho_n = NULL, f = NULL, n, star_t, prop, ...){
#'
#'   gr1 <- sample_sparse_graphon(W, rho_n, n)
#'
#'   if(f %in% c(star_f1, star_f2, star_f3, star_f4, star_f5)){
#'     gr <- add_star_edges(gr1, f, star_t)
#'   }else if(f == add_rings_to_graph){
#'
#'   }
#'
#' }
#'
#'
#' add_star_edges <- function(gr, star_f, t) {
#'   # This function adds star edges to a graph based on a star graph function.
#'   #
#'   # Args:
#'   #   gr: An igraph graph object.
#'   #   star_f: A function representing the starness of nodes in a graph.
#'   #   t: Time
#'   #
#'   # Returns:
#'   #   A modified igraph graph object with added star edges.
#'
#'   num_nodes <- vcount(gr)
#'   star_samples <- sample_star_function(star_f, num_nodes, t)
#'
#'   star_inds <- which(star_samples > 0)
#'   for (i in star_inds) {
#'     num_star_edges <- star_samples[i]
#'     # add num_star_edges vertices to gr
#'     new_vertices <- seq(num_nodes + 1, num_nodes + num_star_edges)
#'     gr <- add_vertices(gr, nv = num_star_edges)
#'     # connect vertex i to new vertices
#'     gr <- add_edges(gr, c(rbind(rep(i, num_star_edges), new_vertices)))
#'     num_nodes <- vcount(gr)
#'   }
#'
#'   return(gr)
#' }
#'
#'
#'
#' star_f1 <- function(xvals, t){
#'   vals <- exp(-1*(xvals - 0.2)*t)
#'   return(vals)
#' }
#'
#' star_f2 <- function(xvals, t){
#'   vals <- t*exp(-1*(xvals - 0.2))
#'   return(vals)
#' }
#'
#'
#' star_f3 <- function(xvals, t){
#'   vals <- t*exp(-1*(xvals + 1))
#'   return(vals)
#' }
#'
#'
#' star_f4 <- function(xvals, t){
#'   k=3
#'   vals <- rep(k*t, length(xvals))
#'   return(vals)
#' }
#'
#'
#' star_f5 <- function(xvals, t){
#'   vals <- t/10*exp(-1*(xvals - 0.2))
#'   return(vals)
#' }
#'
#'
#' add_rings_to_graph <- function(gr, proportion = 0.1, ring_sizes = c(5, 6)) {
#'   # Input validation
#'   if (!is.igraph(gr)) {
#'     stop("Input must be an igraph object")
#'   }
#'   if (proportion <= 0 || proportion > 1) {
#'     stop("Proportion must be between 0 and 1")
#'   }
#'
#'   # Calculate number of rings to add
#'   n_vertices <- vcount(gr)
#'   n_rings <- max(1, round(n_vertices * proportion))
#'
#'   # Start with the original graph
#'   result_graph <- gr
#'
#'   # Add rings
#'   for (i in 1:n_rings) {
#'     # Randomly choose ring size (5 or 6 vertices)
#'     ring_size <- sample(ring_sizes, 1)
#'
#'     # Create a ring
#'     ring <- make_ring(ring_size)
#'
#'     # Get current number of vertices before adding ring
#'     current_n <- vcount(result_graph)
#'
#'     # Combine the graphs (disjoint union)
#'     result_graph <- disjoint_union(result_graph, ring)
#'
#'     # Select a random node from the original graph to connect to
#'     selected_node <- sample(1:current_n, 1)
#'
#'     # Select a random node from the newly added ring to connect from
#'     # The ring vertices are now indexed from (current_n + 1) to (current_n + ring_size)
#'     ring_node <- sample((current_n + 1):(current_n + ring_size), 1)
#'
#'     # Add an edge connecting the selected node to the ring
#'     result_graph <- add_edges(result_graph, c(selected_node, ring_node))
#'   }
#'
#'   return(result_graph)
#' }
