#' @export
higlasso <- function(y, x, z, lambda1, lambda2, sigma, degree = 2,
                       maxit = 1000, eps = 1e-6, QR = TRUE)
{
    y <- y - mean(y)
    generate.Xm <- function(i)
    {
        m <- splines::bs(x[, i], df = degree)
        if (QR)
            m <- qr.Q(qr(m))

        m %*% diag(1 / apply(m, 2, sd))
    }
    Xm <- lapply(1:ncol(x), generate.Xm)

    # QR decompose Xm
    Xi <- generate_Xi(Xm)


  glasso.x <- do.call("cbind", Xm)
  gglasso.groups <- lapply(1:length(Xm), function(i) rep(i, ncol(Xm[[i]])))
  n.groups <- length(Xm)
  n <- length(Xm)
  for (j in 1:n.groups) {
    for (i in 1:n.groups) {
        p <- ncol(Xi[[i,j]])
        if (p > 0) {
          glasso.x <- cbind(glasso.x, Xi[[i, j]])
          n <- n + 1
          gglasso.groups[[n]] <- rep(n, p)
        }
    }
  }

  gglasso.out <- gglasso::gglasso(glasso.x, y, group = unlist(gglasso.groups),
                                   lambda = lambda1 / 2, eps = eps,
                                   maxit = maxit)
  coefs <- gglasso.out$beta

  n <- 1
  higlasso.coefs <- lapply(1:length(gglasso.groups), function(i)
    {
      j <- length(gglasso.groups[[i]])
      ret <- coefs[n:(n + j - 1)]
      n <<- n + j
      ret
    })

  beta <- higlasso.coefs[1:length(Xm)]
  eta  <- higlasso.coefs[-(1:length(Xm))]
  print(system.time({out <- higlasso_internal(y, Xm, Xi, z, beta, eta, lambda1, lambda2, sigma, maxit, eps)}))
    out
}
