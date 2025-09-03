#' Plots the output of extract_sparse function.
#'
#' Plots the two lines fitted to the unique, sorted log degrees of the graph.
#'
#' @param object The output of the function 'extract_sparse'
#' @param ... Other arguments currently ignored
#'
#' @return A ggplot object.
#'
#' @examples
#' library(igraph)
#' gr <- sample_pa(10000, power = 1.2, directed = FALSE)
#' sparse <- extract_sparse(gr)
#' autoplot(sparse)
#'
#' @export
autoplot.extract_sparse <- function(object, ...){
  x <- y <- segment <- NULL

  # Extract data and num_hubs
  df <- object$data
  i <- object$cutoff

  # Create data for the fitted lines
  x_range1 <- seq(1, i, length.out = 100)
  x_range2 <- seq(i+1, nrow(df), length.out = 100)

  # Predict values for each segment
  y_pred1 <- predict(object$models[[1]], newdata = data.frame(x = x_range1))
  y_pred2 <- predict(object$models[[2]], newdata = data.frame(x = x_range2))

  # Create data frames for the fitted lines
  line_df1 <- data.frame(x = x_range1, y = y_pred1, segment = "Sparse")
  line_df2 <- data.frame(x = x_range2, y = y_pred2, segment = "Dense")

  # Combine all line data
  line_df <- rbind(line_df1, line_df2)

  # Create data for elbow point
  elbow_df <- data.frame(
    x = i,
    y = df$y[i]
  )

  # Produce plot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(size = 1) +
    ggplot2::geom_line(data = line_df, aes(x = x, y = y, color = segment), size = 1) +
    ggplot2::theme_bw() +
    xlab("Index") +
    ylab("Log Degree")
  p
}
