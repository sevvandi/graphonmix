#' Separates the dense and sparse part from a (U,W) graphon mixture
#'
#' This function breaks a (U,W) mixture graph into a dense and sparse component.
#'
#' @param grmix The input graph
#'
#' @return a list with the following components:
#' \item{\code{gr_dense}}{The dense component.}
#' \item{\code{gr_sparse}}{The sparse component.}
#' \item{\code{data}}{The original graph.}
#'
#' @examples
#' library(igraph)
#' W <- matrix(0.1, nrow = 100, ncol = 100)
#' wts <- c(0.5, 0.3, 0.2)
#' ns <- 200
#' nd <- 100
#' p <- 0.5
#' gr <- sample_mixed_graph(W, wts, nd, ns, p = 0.1, option = 2)
#' out <- separate_dense_and_sparse(gr)
#' out
#'
#'@export separate_dense_and_sparse
separate_dense_and_sparse <- function(grmix){
  out <- extract_sparse(grmix)
  num_hubs <- out$num_hubs
  sorted_stars <- sort(degree(grmix), decreasing = TRUE)[1:num_hubs]
  gr_dense <- grmix
  for(i in 1:num_hubs){
    st <- igraph::make_star(n = sorted_stars[i], mode = "undirected")
    star_vertex <- which(degree(gr_dense) == sorted_stars[i] )
    gr_dense <- igraph::delete_vertices(gr_dense, star_vertex)

    if(i == 1){
      gr_sparse <- st
    }else{
      gr_sparse <- gr_sparse %du% st
    }
  }

  # Remove isolated edges
  # both endpoints have degree == 1
  ends <- ends(gr_dense, igraph::E(gr_dense))  # matrix of edge endpoints

  isolated_edges <- igraph::E(gr_dense)[degree(gr_dense)[ends[,1]] == 1 &
                           degree(gr_dense)[ends[,2]] == 1]

  gr_dense <- igraph::delete_edges(gr_dense, isolated_edges)

  # Delete isolated vertices from gr_dense
  gr_dense <- igraph::delete.vertices(gr_dense, which(degree(gr_dense) == 0))

  # Return results
  structure(list(
    dense_part = gr_dense,
    sparse_part = gr_sparse,
    data = grmix,
    call = match.call()
  ), class = 'separate_dense_and_sparse')
}
