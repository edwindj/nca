#' neighborhood component analysis
#'
#' neighborhood component analysis
#' @param x numeric data that can be coerced into a numeric matrix
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

  for (it in seq_len(N_iter)){
    for (i in seq_len(N)){
      # softmax, with LOO
      D <- tcrossprod(A, x)          # (N, d)
      D <- (D - as.integer(D[,i]))
      Ad <- exp(-colSums(D*D))       # (N)

      Ad[i] <- 0
      p_ik <- Ad/sum(Ad)             # (N)
      # end softmax

      # neighbors that predict the correct label
      correct_label <- labels == labels[i]  # (N)

      p[i] <- sum(p_ik[correct_label])
      d <- t(x) - as.integer(x[i,])  # (N, d)
      pd <- p_ik * d
      #browser(expr={i == 61})

      g <- p[i]*tcrossprod(d, pd) - tcrossprod(d[,correct_label], pd[,correct_label])
      A <- A + learning_rate * (A %*% g)
      #A <- tcrossprod(A, d)
  }
  }
  list(A=A, p = p, g=g)
}


x <- iris[1:4]
labels <- iris[[5]]
A <- diag(ncol(x))
#A <- matrix(0.1, ncol=4, nrow=2)

nca(x=x, labels = labels, A_init = A, N_iter = 1)
