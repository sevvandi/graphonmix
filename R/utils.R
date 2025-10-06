arrange_matrix <- function(mat){
  rowsums <- apply(mat, 1, sum)
  ord <- order(rowsums, decreasing = TRUE)
  mat2 <- mat[ord, ]
  colsums <- apply(mat, 2, sum)
  ord <- order(colsums, decreasing = TRUE)
  mat3 <- mat2[ ,ord]
  mat3
}

#' Plots graphon
#'
#' Plots graphon
#'
#' @param W A graphon given by an nxn matrix
#' @param cols Colors, by default white and black
#'
#' @return A ggplot object.
#'
#' @examples
#' W <- create_exp_matrix(100, 100)
#' plot_graphon(W)
#'
#' @export
plot_graphon <- function(W, cols=c("white","black")){
  column <- val <- NULL
  adj1 <- W
  df1 <- data.frame(val = c(matrix(adj1)),
                    row = rep(rep(1:dim(adj1)[1],each = dim(adj1)[1]),
                              len = length(adj1)),
                    column = rep(1:dim(adj1)[1], len = length(adj1)))
  g2 <- ggplot(df1, aes(x = row, y = column, fill = val)) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_rect(xmin = min(df1$row)-0.5 , xmax = max(df1$row)+0.5 ,
              ymin = -1*max(df1$column)-0.5 , ymax = -1*min(df1$column)+0.5 ,
              fill = NA, color = "black", size = 0.5, inherit.aes = FALSE) +
    ggplot2::scale_fill_gradientn(colours = cols) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  g2
}

#' Creates an nxn exponential matrix
#'
#' Creates an nxn matrix where the (i,j)th entry is exp(-(i+j)/scalar)
#'
#' @param nrow The dimension of the matrix
#' @param scalar The scalar in exp(-(i+j)/scalar)
#'
#' @return An nxn matrix
#'
#' @examples
#' W <- create_exp_matrix(100, 100)
#'
#' @export
create_exp_matrix <- function(nrow, scalar) {
  # Initialize an empty matrix of the desired dimensions
  result <- matrix(0, nrow = nrow, ncol = nrow)

  # Fill each (i,j) position with exp(-i-j)
  for (i in 1:nrow) {
    for (j in 1:nrow) {
      result[i, j] <- exp(-(i+j)/scalar)
    }
  }

  return(result)
}



