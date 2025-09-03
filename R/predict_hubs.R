#' Predicts the degree of hubs of an new graph
#'
#' Predicts the degree of hubs of an unseen graph given a graph generated
#' from the same process.
#'
#' @param gr The input graph
#' @param n The number of nodes in the new graph
#' @param k The number of hubs. Default is \code{NULL}
#'
#' @return A vector of hub degrees
#'
#' @examples
#' library(igraph)
#' gr <- sample_pa(10000, power = 1.2, directed = FALSE)
#' predict_hubs(gr, n = 11000)
#' @export
predict_hubs <- function(gr, n, k = NULL){
  # gr is the current graph
  # n is the number of nodes of the new graph
  # k is the number of hubs
  if(is.null(k)){
    sparse <- extract_sparse(gr)
    k <- sparse$num_hubs
  }
  current_hub_degs <- sort(igraph::degree(gr), decreasing = TRUE)[1:k]
  num_nodes <- igraph::vcount(gr)
  new_hub_degs <- round(current_hub_degs/num_nodes*n)
  new_hub_degs

}
