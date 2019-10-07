#' Hierarchical Integrative Group LASSO
#'
#' TODO
#'
#' We have designed \code{higlasso} to
#' \itemize{
#'   \item Impose strong heredity constraints on two-way interaction effects
#'       (hierarchical).
#'   \item Incorporate adaptive weights without necessitating initial
#'       coefficient estimates.
#'   \item Induce sparsity for variable selection while respecting group
#'       structure (group LASSO).
#' }
#' @param Y A length n numeric response vector
#' @param X A n x p numeric matrix
#' @param Z A n x m numeric matrix
#' @param lambda1 Penalty for main effects
#' @param lambda2 Penalty for interaction effects
#' @param sigma Scale parameter for Integrative weights. Technically a third
#'     tuning paramter but defaults to 1 for computational tractibility
#' @param Xm An optional list of design matrices if the user wishes to use their
#'     own groupings instead of a basis expansion
#' @param degree Degree of \code{bs} basis expansion. Default is 2
#' @param maxit Maximum number of iterations. Default is 1000
#' @param eps Numeric tolerance for convergence. Defaults to 1e-6
#' @param tol Tolerance for RcppArmadillo solve. This is separate from \code{eps}.
#'     Default is machine epsilon
#' @param faster If TRUE, higlasso will be faster, but possibly less accurate.
#'     Default is FALSE
#' @examples TODO
#' @author Alexander Rix
#' @export
higlasso <- function(Y, X, Z, lambda1, lambda2, sigma = 1, Xm = NULL,
                         degree = 2, maxit = 1000, eps = 1e-6, tol =
                         .Machine$double.eps, faster = FALSE)
{
    if (!is.vector(Y) || !is.numeric(Y))
        stop("'Y' must be a numeric vector.")
    if (any(is.na(Y)))
        stop("'Y' cannot contain missing values.")

    # We need to check to see if Xm is null first incase the user wanted to
    # specify their own groups. In that case X is missing so we don't want to
    # refer to it.
    if (!is.null(Xm)) {
        if (!is.list(Xm))
            stop("'Xm' must be a list.")

        lapply(1:length(Xm), function(i)
        {
            if (!is.matrix(Xm[[i]]) || !is.numeric(Xm[[i]]))
                stop(paste0("'Xm[[", i, "]]' must be a numeric vector."))
            if (nrow(Xm[[i]]) != length(Y))
                stop(paste0("The number of rows of 'Xm[[", i,
                            "]]' does not match the length of 'Y'."))
            if (any(is.na(Xm[[i]])))
                stop(paste0("'Xm[[", i, "]]' cannot contain missing values."))
        })
    }
    else {
        if (!is.matrix(X) || !is.numeric(X))
            stop("'X' must be a numeric vector.")
        if (nrow(X) != length(Y))
            stop("The number of rows of 'X' does not match the length of 'Y'.")
        if (any(is.na(X)))
            stop("'X' cannot contain missing values.")
    }

    if (!is.matrix(Z) || !is.numeric(Z))
        stop("'Z' must be a numeric vector.")
    if (nrow(Z) != length(Y))
        stop("The number of rows of 'Z' does not match the length of 'Y'.")
    if (any(is.na(Z)))
        stop("'Z' cannot contain missing values.")

    if (!is.numeric(lambda1) || lambda1 < 0)
        stop("'lambda1' must be a nonnegative number.")
    if (length(lambda1) > 1)
        stop("'lambda1' must have unit length.")

    if (!is.numeric(lambda2) || lambda2 < 0)
        stop("'lambda2' must be a nonnegative number.")
    if (length(lambda2) > 1)
        stop("'lambda2' must have unit length.")

    if (!is.numeric(sigma) || sigma < 0)
        stop("'sigma' must be a nonnegative number.")
    if (length(sigma) > 1)
        stop("'sigma' must have unit length.")

    if (!is.numeric(degree) || degree < 1)
        stop("'degree' should be an integer >= 1.")

    if (!is.numeric(maxit) || maxit < 1)
        stop("'maxit' should be an integer >= 1.")

    if (!is.numeric(eps) || eps <= 0)
        stop("'eps' should be a postive number.")

    if (!is.logical(faster))
        stop("'faster' must take on a logical value.")

    if (is.null(Xm)) {
        generate.Xm <- function(i)
        {
            m <- splines::bs(X[, i], degree = degree)
            m <- qr.Q(qr(m))
            apply(m, 2, function(x) x / stats::sd(x))
        }

        Xm <- lapply(1:ncol(X), generate.Xm)
        if (is.null(colnames(X)))
            colnames(X) <- 1:ncol(X)
        names(Xm) <- paste0("bs(", colnames(X), ")")
    }
    if (is.null(names(Xm)))
        names(Xm) <- paste0("G", 1:length(Xm))

    Xi <- generate_Xi(Xm)

    X.init <- do.call("cbind", Xm)
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
    X.init  <- cbind(X.init, Z)
    weights <- c(rep(1, nx), rep(0, ncol(Z)))

    e.net <- gcdnet::gcdnet(X.init, Y, method = "ls", lambda2 = lambda2,
                                pf = weights, pf2 = weights, eps = eps,
                                maxit = max(maxit, 1e6))

    if (e.net$jerr != 0)
        stop("Error in gcdnet::gcdnet.")

    # get the best scoring lambda from gcdnet and use that to generate inital
    # weights for the adpative elastic net
    mse <- function(p) mean((p - Y)^2)
    i <- which.min(apply(stats::predict(e.net, X.init), 2, mse))

    ada.e.weights <- 1 / (e.net$beta[1:nx, i] + 1 / nrow(X.init))
    ada.e.weights <- c(ada.e.weights, rep(0, ncol(Z)))


    ada.e.net <- gcdnet::gcdnet(X.init, Y, method = "ls", lambda = lambda1,
                                    lambda2 = lambda2, pf = ada.e.weights,
                                    eps = eps, maxit = max(maxit, 1e6))

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
    higlasso.out <- higlasso_internal(Y, Xm, Xi, Z, beta, eta, lambda1, lambda2,
                                          sigma, maxit, eps, tol, faster)

    n <- length(Xm)
    names(higlasso.out$beta) <- names(Xm)
    for (i in 1:n)
        higlasso.out$beta[[i]] <- round(higlasso.out$beta[[i]], 9)
    for (j in 1:n)
        for (i in 1:n)
            higlasso.out$eta[[i, j]] <- round(higlasso.out$eta[[i, j]], 9)

    higlasso.out$Y <- Y
    higlasso.out$Xm <- Xm
    higlasso.out$Xi <- Xi
    higlasso.out$Z <- Z
    higlasso.out$degree <- degree
    higlasso.out$lambda <- c(lambda1, lambda2)

    class(higlasso.out) <- "higlasso"
    higlasso.out
}

#' Print higlasso fits
#' @param x An object of type 'higlasso'
#' @param ... Additional parameters to pass onto print
#' @export
print.higlasso <- function(x, ...)
{
    if (class(x) != "higlasso")
        stop("'object' is not a higlasso fit.")

    sum <- summary.higlasso(x)

    cat("HiGLASSO fit\n")
    cat("Selected main effects:\n")
    print(sum$main, zero.print = "", right = F)
    cat("Selected interaction effects:\n")
    print(sum$inter, zero.print = "", right = F)
    cat(sprintf("Mean squared prediction error: %f\n", sum$mspe))
    cat("Lambda = ")
    cat(sum$lambda)
    cat("\n")
}

#' Summarise higlasso fits
#' @param object An object of type 'higlasso'
#' @param ... Additional parameters to pass on.
#' @export
summary.higlasso <- function(object, ...)
{
    if (class(object) != "higlasso")
        stop("'object' is not a higlasso fit.")

    main <- sapply(object$beta, function(b) ifelse(any(b != 0), 1, 0))
    n <- length(object$Xm)
    inter <- matrix(NA, n, n)
    for (j in 1:n)
        for (i in 1:n)
            if (nrow(object$eta[[i, j]]) > 0) {
                if (any(object$eta[[i, j]] != 0))
                    inter[i , j] <- 1
                else
                    inter[i, j] <- 0
            }
    colnames(inter) <- rownames(inter) <- names(object$beta)
    list(main = as.table(main), inter = as.table(t(inter)), mspe = object$mspe,
         lambda = object$lambda)
}

#' Predict via a higlasso fit
#' @param object An object of type 'higlasso'
#' @param newdata An optional list of length 3, conataining new Y, new Xm, and
#'     new Z. For experts only.
#' @param ... Additonal parameters to pass on
#' @export
predict.higlasso <- function(object, newdata, ...)
{
    if (class(object) != "higlasso")
        stop("'object' is not a higlasso fit.")
    n.groups <- length(object$Xm)
    beta <- object$beta
    eta  <- object$eta

    if (!missing(newdata)) {
        if (!is.list(newdata) || length(newdata) != 3)
            stop("'newdata' should be a length 3 list.")
        Y  <- newdata[[1]]
        Xm <- newdata[[2]]
        Xi <- generate_Xi(Xm)
        Z  <- newdata[[3]]
    }
    else {
        Y <- object$Y
        Xm  <- object$Xm
        Xi  <- object$Xi
        Z <- object$Z
    }
    res <- Y - Z %*% object$alpha
    for (i in 1:n.groups)
        res <- res - (Xm[[i]] %*% beta[[i]])

    for (j in 1:n.groups)
        for (i in 1:n.groups) {
            if (nrow(eta[[i, j]]) > 0) {
                e <- eta[[i, j]] * kronecker(beta[[i]], beta[[j]])
                res <- res - Xi[[i, j]] %*% e
            }
        }
    0.5 * mean(res * res)
}
