#' Plots the output of separate_dense_and_sparse.
#'
#' Plots the three lines fitted to the unique, sorted log degrees of the graph.
#'
#' @param object The output of the function 'separate_dense_and_sparse.'
#' @param ... Other arguments currently ignored
#'
#' @return A ggplot object.
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
#' separate <- separate_dense_and_sparse(gr)
#' autoplot(separate)
#'
#' @export
autoplot.separate_dense_and_sparse <- function(object, ...) {
  x <- y <- segment <- NULL

  # Extract data and cutoffs
  result <- object$fitlines
  df <- result$data
  i <- result$cutoffs[1]
  j <- result$cutoffs[2]

  # Create data for the fitted lines
  x_range1 <- seq(1, i, length.out = 100)
  x_range2 <- seq(i+1, j, length.out = 100)
  x_range3 <- seq(j+1, nrow(df), length.out = 100)

  # Predict values for each segment
  y_pred1 <- predict(result$models[[1]], newdata = data.frame(x = x_range1))
  y_pred2 <- predict(result$models[[2]], newdata = data.frame(x = x_range2))
  y_pred3 <- predict(result$models[[3]], newdata = data.frame(x = x_range3))

  # Create data frames for the fitted lines
  line_df1 <- data.frame(x = x_range1, y = y_pred1, segment = "Segment 1")
  line_df2 <- data.frame(x = x_range2, y = y_pred2, segment = "Segment 2")
  line_df3 <- data.frame(x = x_range3, y = y_pred3, segment = "Segment 3")

  # Combine all line data
  line_df <- rbind(line_df1, line_df2, line_df3)

  # Create data for elbow points
  elbow_df <- data.frame(
    x = c(i, j),
    y = df$y[c(i, j)]
  )

  # Create the plot
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_point(size = 2) +
    geom_line(data = line_df, aes(x = x, y = y, color = segment), size = 1) +
    geom_point(data = elbow_df, aes(x = x, y = y), color = "purple", size = 3, shape = 19) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = c("Segment 1" = "red", "Segment 2" = "blue", "Segment 3" = "green"),
      name = "Lines"
    ) +
    ggplot2::annotate(
      "text",
      x = c(i, j),
      y = df$y[c(i, j)],
      label = c("Elbow 1", "Elbow 2"),
      vjust = -1,
      hjust = 0.5
    ) +
    ggplot2::labs(x = "Index", y = "Log Degree")

  return(p)
}


