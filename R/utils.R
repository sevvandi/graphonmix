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
    ggplot2::scale_fill_gradientn(colours = cols,
                                  limits = c(0,1)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  g2
}

#' Creates an exponential graphon
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


#' Creates a ring graphon
#'
#' Creates an graphon which can generate ring graphs.
#'
#' @param n The dimension of the matrix
#' @param alpha A scaling factor used in the graphon. Default set to 0.1
#'
#' @return An nxn matrix
#'
#' @examples
#' # example code
#' W <- ring_graphon(100)
#' plot_graphon(W)
#' @export
ring_graphon <- function(n, alpha = 0.1){
  mat <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    x <- i/n
    y <- (1:n)/n
    mat[i, ] <- 0.9*exp((-y^2 - (x-1)^2)/alpha^2) + 0.9*exp((-(y-1)^2 - x^2)/alpha^2) + 0.9*exp(-((sin(0.75*pi)*x + cos(0.75*pi)*y )/alpha)^2)
  }
  mat
}


#' Creates a Stochastic Block Model graphon
#'
#' Creates an graphon representing a Stochastic Block Model (SBM).
#'
#' @param mat The matrix representing the SBM
#' @param n The dimension of the matrix
#'
#' @return An nxn matrix
#'
#' @examples
#' # example code
#' mat <- matrix(c(0.9, 0.01, 0.02,
#' 0.01, 0.8, 0.03,
#' 0.02, 0.03, 0.7), nrow = 3, byrow = TRUE)
#' W <- sbm_graphon(mat, 100)
#' plot_graphon(W)
#' @export
sbm_graphon <- function(mat, n){
  k <- nrow(mat)
  block_size <- n / k
  graphon <- matrix(0, nrow = n, ncol = n)

  for(i in 1:k){
    for(j in 1:k){
      row_start <- floor((i - 1) * block_size) + 1
      row_end <- floor(i * block_size)
      col_start <- floor((j - 1) * block_size) + 1
      col_end <- floor(j * block_size)

      graphon[row_start:row_end, col_start:col_end] <- mat[i, j]
    }
  }

  return(graphon)
}
