#' Extracts the sparse part from a (U,W) graphon mixture
#'
#' This function extracts the sparse component from a (U,W) mixture graph by
#' fitting two lines to the unique sorted log degree values. The first line gives
#' the number of hubs in the sparse part and the remaining line fits the degrees
#' of the dense part.
#'
#' @param gr The input graph
#'
#' @return a list with the following components:
#' \item{\code{num_hubs}}{The number of hubs in the sparse component.}
#' \item{\code{phat}}{The probability vector of the sparse component. This is also known as the mass partition.}
#' \item{\code{segment_sizes}}{The sizes of the two line segments.}
#' \item{\code{line_equations}}{The two equations of the lines.}
#' \item{\code{cutoff}}{The best cut off for the two lines.}
#' \item{\code{models}}{The models of the fitted lines. }
#' \item{\code{mse}}{The mean squared error}
#' \item{\code{data}}{The degree data.}
#'
#' @examples
#' library(igraph)
#' gr <- sample_pa(10000, power = 1.2, directed = FALSE)
#' sparse <- extract_sparse(gr)
#' sparse$phat
#'
#'@export extract_sparse
extract_sparse <- function(gr){
  # gr is a graph
  degs_nonuniq <- sort(igraph::degree(gr), decreasing = TRUE)
  degs <- sort(unique(igraph::degree(gr)), decreasing = TRUE)
  log_degs <- log(degs + 1)
  log_degs <- log_degs[log_degs > median(log_degs)]
  out <- fit_two_segments(log_degs)
  kk <- out$num_hubs
  kknew <- sum(degs_nonuniq >= degs[kk])  # Adjust num_hubs to be the number of points above the elbow point
  out$num_hubs <- kknew

  # Get the mass-partition (sparse component probabilities)
  degree_sum <- sum(sort(degree(gr), decreasing = TRUE)[1:kknew])
  phat <- sort(degree(gr), decreasing = TRUE)[1:kknew]/degree_sum

  out$phat <- phat
  out
}


fit_two_segments <- function(y) {
  # Create x values from 1 to length(y)
  x <- 1:length(y)
  n <- length(y)

  # Create a data frame with the points
  df <- data.frame(x = x, y = y)

  # Initialize variables to store best fit
  best_mse <- Inf
  best_i <- 0
  best_models <- list()

  # Try all possible changepoints
  for (i in 2:(n-1)) {
    # Segment 1: points 1 to i
    segment1 <- lm(y ~ x, data = df[1:i, ])

    # Segment 2: points (i+1) to n
    segment2 <- lm(y ~ x, data = df[(i+1):n, ])

    # Calculate mean squared error for this combination
    pred1 <- predict(segment1, newdata = df[1:i, ])
    pred2 <- predict(segment2, newdata = df[(i+1):n, ])

    mse1 <- mean((df$y[1:i] - pred1)^2)
    mse2 <- mean((df$y[(i+1):n] - pred2)^2)

    # Weighted average MSE based on segment sizes
    mse <- (i * mse1 + (n - i) * mse2) / n

    # Update if this is the best fit so far
    if (mse < best_mse) {
      best_mse <- mse
      best_i <- i
      best_models <- list(segment1, segment2)
    }
  }

  # Extract coefficients from the best models
  coef1 <- coef(best_models[[1]])
  coef2 <- coef(best_models[[2]])

  # Format the line equations
  line1_eq <- sprintf("y = %.4f * x + %.4f", coef1[2], coef1[1])
  line2_eq <- sprintf("y = %.4f * x + %.4f", coef2[2], coef2[1])


  # Return results
  structure(list(
    num_hubs = best_i,
    phat = NULL,
    cutoff = best_i,
    segment_sizes = c(best_i, n - best_i),
    line_equations = c(line1_eq, line2_eq),
    models = best_models,
    mse = best_mse,
    data = df,
    call = match.call()
  ), class = 'extract_sparse')


}
