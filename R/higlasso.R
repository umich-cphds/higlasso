#' Hierarchical Integrative Group LASSO
#'
#' @param Y A length n numeric response vector
#' @param X A n x p numeric matrix
#' @param Z A n x m numeric matrix
#' @param lambda1 Penalty for main effects
#' @param lambda2 Penalty for interaction effects
#' @param sigma Scale parameter for Integrative weights
#' @param degree Degree of \code{bs} basis expansion. Default is 2
#' @param maxit Maximum number of iterations. Default is 1000
#' @param eps Numeric tolerance for convergence. Defaults to 1e-6
#' @param QR Whether or not to QR decompose the design matrix. Default is TRUE
#' @examples TODO
#' @author Alexander Rix
#' @export
higlasso <- function(Y, X, Z, lambda1, lambda2, sigma, degree = 2,
                       maxit = 1000, eps = 1e-6, QR = TRUE)
{
    Y <- Y - mean(Y)

    generate.Xm <- function(i)
    {
        m <- splines::bs(X[, i], degree = degree)
        if (QR)
            m <- qr.Q(qr(m))

        m %*% diag(1 / apply(m, 2, sd))
    }

    Xm <- lapply(1:ncol(X), generate.Xm)

    # QR decompose Xm
    Xi <- generate_Xi(Xm)


    X.init <- do.call("cbind", Xm)
    print(ncol(X.init))
    higlasso.groups <- lapply(1:length(Xm), function(i) rep(i, ncol(Xm[[i]])))
    n.groups <- length(Xm)
    n <- length(Xm)
    for (j in 1:n.groups) {
        for (i in 1:n.groups) {
            p <- ncol(Xi[[i, j]])
            if (p > 0) {
                X.init <- cbind(X.init, Xi[[i, j]])
                n <- n + 1
                higlasso.groups[[n]] <- rep(n, p)
            }
        }
    }

    nx      <- ncol(X.init)
    print(nx)
    X.init  <- cbind(X.init, Z)
    weights <- c(rep(1, nx), rep(0, ncol(Z)))

    e.net <- gcdnet::gcdnet(X.init, Y, method = "ls", lambda2 = lambda2,
                                pf = weights, pf2 = weights, eps = eps,
                                maxit = maxit)

    if (e.net$jerr > 0)
        stop("Error in gcdnet::gcdnet.")

    i <- which.min(apply(predict(e.net, X.init), 2, function(p) mean((p - y)^2)))

    ada.e.weights <- 1 / (e.net$beta[1:nx, i] + 1 / nrow(X.init))
    ada.e.weights <- c(ada.e.weights, rep(0, ncol(Z)))


    ada.e.net <- gcdnet::gcdnet(X.init, y, method = "ls", lambda = lambda1,
                                    lambda2 = lambda2, pf = ada.e.weights,
                                    eps = eps, maxit = maxit)


  coefs <- ada.e.net$beta[1:nx]

  n <- 1
  higlasso.coefs <- lapply(1:length(higlasso.groups), function(i)
    {
      j <- length(higlasso.groups[[i]])
      ret <- coefs[n:(n + j - 1)]
      n <<- n + j
      ret
    })

  beta <- higlasso.coefs[1:length(Xm)]
  eta  <- higlasso.coefs[-(1:length(Xm))]
  higlasso_internal(y, Xm, Xi, Z, beta, eta, lambda1, lambda2, sigma, maxit, eps)
}
