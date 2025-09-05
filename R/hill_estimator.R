#' Computes the Hill estimator or the power of the degree distribution
#'
#' Given a graph, computes the Hill estimator or the power of the
#' degree distribution.
#'
#' @param gr A graph
#' @param min_deg The minimum degree to consider. Default set to 5
#'
#' @return The Hill estimate
#'
#' @examples
#' library(igraph)
#' gr <- sample_pa(10000,  directed = FALSE)
#' hill_estimator(gr)
#'
#'
#' @export
hill_estimator <- function(gr, min_deg = 2){
  sorted_degs <-  sort(igraph::degree(gr))
  unique_degs <- unique(sorted_degs)
  inds <- which(sorted_degs >= min_deg)
  tail_degs <- sorted_degs[inds]

  df <- as.data.frame(table(tail_degs))

  dmin <- (min(tail_degs)-1)
  j <- sort(unique(tail_degs))
  d_j <- df$Freq

  numerator <- sum(d_j*log(j/dmin))
  # or
  # numerator <- sum(tail_degs*log(inds/min(tail_degs)))

  denominator <- sum(d_j)
  numerator/denominator
}
