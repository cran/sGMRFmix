#' Compute anomaly scores
#'
#' @param obj object
#' @param x data.frame to compute anomaly scores
#' @param window_size integer.
#' @param ... additional arguments
#'
#' @return matrix of anomaly scores
#'
#' @examples
#' library(sGMRFmix)
#'
#' set.seed(314)
#' train_data <- generate_train_data()
#' fit <- sGMRFmix(train_data, K = 7, rho = 10)
#'
#' test_data <- generate_test_data()
#' compute_anomaly_score(fit, test_data)
#'
#' @export
compute_anomaly_score <- function(obj, x, window_size = 1L, ...) {
  UseMethod("compute_anomaly_score")
}

#' @importFrom stats dnorm
#' @importFrom zoo rollmeanr
#'
#' @export
compute_anomaly_score.sGMRFmix <- function(obj, x, window_size = 1L, ...) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  m <- obj$m
  A <- obj$A
  theta <- obj$theta

  N <- nrow(x)
  M <- ncol(x)
  K <- length(m)

  x <- as.matrix(x)
  w <- compute_variance(A)
  u <- compute_mean(x, m, A, w)
  anomaly_score <- matrix(nrow = N, ncol = M)
  for (i in 1:M) {
    g <- compute_gating_function(x, theta, u, w, i)$g
    tmp <- lapply(1:K, function(k) {
      g[, k] * dnorm(x[,i], mean = u[[k]][,i], sd = w[[k]][i])
    })
    anomaly_score[,i] <- -log(rowSums(do.call(cbind, tmp)))
  }

  anomaly_score <- as.data.frame(anomaly_score)
  colnames(anomaly_score) <- colnames(x)

  if (window_size == 1L) {
    anomaly_score
  } else {
    rollmeanr(anomaly_score, k = window_size, fill = "extend")
  }

}
