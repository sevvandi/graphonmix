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
                    row = rep(rep(1:dim(adj1)[1], each = dim(adj1)[1]), len = length(adj1)),
                    column = rep(1:dim(adj1)[1], len = length(adj1))
  )
  g2 <- ggplot(df1, aes(x = row, y = column, fill = val)) +
    geom_raster() +
    scale_fill_gradientn(colours=cols) +
    theme(legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_y_reverse()
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
