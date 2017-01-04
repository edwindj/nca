#' neighborhood component analysis
#'
#' Neighborhood Component Analysis
#' @param x numeric data that can be coerced into a numeric matrix
#' @param labels \code{factor} with the "correct" labels for each row in \code{x}
#' @param A_init start scale matrix \code{A}
#' @param N_iter \code{numeric} number of iterations
#' @param learning_rate rate in which change are made to matrix \code{A}
#'
#' @return list with rescale matrix \code{A}
nca <- function( x
               , labels
               , A_init = diag(ncol(x))
               , N_iter=1e2
               , learning_rate = 0.01
               ){
  x <- as.matrix(x)
  #labels <- as.factor(labels)

  A <- A_init

  N <- nrow(x)
  stopifnot(NROW(x) == length(labels))

  p <- numeric(N)
  p_cum <- numeric(N_iter)
  for (it in seq_len(N_iter)){
    for (i in seq_len(N)){
      # softmax, with LOO
      D <- tcrossprod(A, x)       # (dA, N)
      D <- (D - as.numeric(D[,i]))
      p_ik <- exp(-colSums(D*D))       # (N)

      p_ik[i] <- 0
      softmax <- sum(p_ik)
      if (softmax > .Machine$double.eps){
        p_ik <- p_ik/sum(p_ik)             # (N)
      }
      # end softmax

      # neighbors that predict the correct label
      correct_label <- labels == labels[i]  # (N)

      p[i] <- sum(p_ik[correct_label])
      d    <- t(t(x) - as.numeric(x[i,]))  # (N, dx)
      pd <- p_ik * d                    # (N, dx)

      g <- (p[i]*crossprod(d, pd)) - crossprod(d[correct_label,], pd[correct_label,]) # (dx, dx)
      A <- A + learning_rate * (A %*% g) # (dx, dA)
      # d  <- t(x) - as.numeric(x[i,])  # (dx, N)
      # d2 <- p_ik * colSums(d * d) # (N)
      #
      # A <- A + learning_rate * A * (p[i]*sum(d2) - sum(d2[correct_label]))
    }
    p_cum[it] <- sum(p)
  }

  list( A = A
      , p = p
      , A_norm = A/A[1,1]
      , p_cum=p_cum
      )
}

scaling <- function(x){
  x_min <- apply(x, 2, min)
  x <- sweep(x, 1, x_min)
  x_max <- apply(x, 2, max)
  diag(1/(x_max))
}

x <- iris[1:4]
x <- as.matrix(x)
labels <- iris[[5]]
A <- diag(ncol(x))
A <- scaling(x)
#pca <- prcomp(x)
#A <- t(pca$rotation[,1:2])
#A <- matrix(runif(4*2), ncol=4, nrow=4)
res <- nca(x=x, labels = labels, A_init = A, N_iter = 200, learning_rate = 1e-2)
res$A_norm
#
# # 2d projection
# x_2d <- t(tcrossprod(res$A, x))
# x_2d <- as.data.frame(x_2d)
#
# plot(x_2d, col=iris$Species)
