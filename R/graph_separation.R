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
  out <- extract_sparse_and_dense(grmix)
  num_hubs <- out$num_hubs
  sorted_stars <- sort(degree(grmix), decreasing = TRUE)[1:num_hubs]
  for(i in 1:num_hubs){
    st <- igraph::make_star(n = sorted_stars[i], mode = "undirected")
    star_vertex <- which(degree(grmix) == sorted_stars[i] )
    if(i == 1){
      gr_sparse <- st
    }else{
      gr_sparse <- gr_sparse %du% st
    }
  }

  # Remove isolated edges
  # both endpoints have degree == 1
  gr_dense <- out$dense_part
  ends <- igraph::ends(gr_dense, igraph::E(gr_dense))  # matrix of edge endpoints

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
    fitlines = out,
    call = match.call()
  ), class = 'separate_dense_and_sparse')
}


extract_sparse_and_dense <- function(gr){
  # gr is a graph
  degs_nonuniq <- sort(igraph::degree(gr), decreasing = TRUE)
  degs <- sort(unique(igraph::degree(gr)), decreasing = TRUE)
  log_degs <- log(degs + 2)
  #log_degs <- log_degs[log_degs > median(log_degs)]
  out <- fit_three_segments(log_degs)
  kk <- out$num_hubs
  kknew <- sum(degs_nonuniq >= degs[kk])  # Adjust num_hubs to be the number of points above the elbow point
  out$num_hubs <- kknew

  # Get the mass-partition (sparse component probabilities)
  degree_sum <- sum(sort(degree(gr), decreasing = TRUE)[1:kknew])
  phat <- sort(degree(gr), decreasing = TRUE)[1:kknew]/degree_sum
  out$phat <- phat

  # dense part
  degs_dense <- degs_nonuniq[(degs_nonuniq < degs[kk]) & (degs_nonuniq >= degs[out$dense_limit])]
  dense_nodes <- which(degree(gr) %in% degs_dense)
  dense_part <- igraph::subgraph(gr, dense_nodes)
  out$dense_part <- dense_part
  out
}



fit_three_segments <- function(y) {
  # Create x values from 1 to length(y)
  x <- 1:length(y)
  n <- length(y)

  # Create a data frame with the points
  df <- data.frame(x = x, y = y)

  # Initialize variables to store best fit
  best_mse <- Inf
  best_i <- 0
  best_j <- 0
  best_models <- list()

  # Try all possible combinations of changepoints
  for (i in 2:(n-2)) {
    for (j in (i+1):(n-1)) {
      # Segment 1: points 1 to i
      segment1 <- lm(y ~ x, data = df[1:i, ])

      # Segment 2: points (i+1) to j
      segment2 <- lm(y ~ x, data = df[(i+1):j, ])

      # Segment 3: points (j+1) to n
      segment3 <- lm(y ~ x, data = df[(j+1):n, ])

      # Calculate mean squared error for this combination
      pred1 <- predict(segment1, newdata = df[1:i, ])
      pred2 <- predict(segment2, newdata = df[(i+1):j, ])
      pred3 <- predict(segment3, newdata = df[(j+1):n, ])

      mse1 <- mean((df$y[1:i] - pred1)^2)
      mse2 <- mean((df$y[(i+1):j] - pred2)^2)
      mse3 <- mean((df$y[(j+1):n] - pred3)^2)

      # Weighted average MSE based on segment sizes
      mse <- (i * mse1 + (j - i) * mse2 + (n - j) * mse3) / n

      # Update if this is the best fit so far
      if (mse < best_mse) {
        best_mse <- mse
        best_i <- i
        best_j <- j
        best_models <- list(segment1, segment2, segment3)
      }
    }
  }

  # Extract coefficients from the best models
  coef1 <- coef(best_models[[1]])
  coef2 <- coef(best_models[[2]])
  coef3 <- coef(best_models[[3]])

  # Format the line equations
  line1_eq <- sprintf("y = %.4f * x + %.4f", coef1[2], coef1[1])
  line2_eq <- sprintf("y = %.4f * x + %.4f", coef2[2], coef2[1])
  line3_eq <- sprintf("y = %.4f * x + %.4f", coef3[2], coef3[1])

  # Return results
  result <- list(
    num_hubs = best_i,
    dense_limit = best_j,
    cutoffs = c(best_i, best_j),
    segment_sizes = c(best_i, best_j - best_i, n - best_j),
    elbow_points = c(best_i, best_j),
    line_equations = c(line1_eq, line2_eq, line3_eq),
    models = best_models,
    mse = best_mse,
    data = df
  )

  return(result)
}
