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
#' @param Y.train A length n numeric response vector. Training set
#' @param X.train A n x p numeric matrix. Training set
#' @param Z.train A n x m numeric matrix. Training set
#' @param Y.test A length n' numeric response vector
#' @param X.test A n' x p numeric matrix. Test set
#' @param Z.test A n' x m numeric matrix. Test set
#' @param lambda1 A numeric vector of main effect penalty tuning parameters. By
#'     default, \code{lambda1 = NULL} and generates a sequence (length
#'     \code{n.lambda1}) of lambda1s based off of the data and
#'     \code{min.lambda.ratio}.
#' @param lambda2 Penalty for interaction effects
#' @param n.lambda1 Number that determines the length of the higlasso generated
#'     \code{lambda1} sequence.
#' @param n.lambda2 Penalty for interaction effects
#' @param lambda.min.ratio ratio that determines min lambda from max lambda.
#' @param sigma Scale parameter for integrative weights. Technically a third
#'     tuning parameter but defaults to 1 for computational tractibility
#' @param degree Degree of \code{bs} basis expansion. Default is 3
#' @param maxit Maximum number of iterations. Default is 2000
#' @param delta Tolerance for convergence. Defaults to 1e-5
#' @examples TODO
#' @author Alexander Rix
#' @export
higlasso <- function(Y.train, X.train, Z.train, Y.test = NULL, X.test = NULL,
                        Z.test = NULL, lambda1 = NULL, lambda2 = NULL,
                        n.lambda1 = 10, n.lambda2 = 10, lambda.min.ratio = .1,
                        sigma = 1, degree = 3, maxit = 5000, delta = 1e-5)
{
    if (!is.vector(Y.train) || !is.numeric(Y.train))
        stop("'Y.train' must be a numeric vector.")
    if (any(is.na(Y.train)))
        stop("'Y.train' cannot contain missing values.")

    if (!is.null(Y.test)) {
        if (!is.vector(Y.test) || !is.numeric(Y.test))
            stop("'Y.test' must be a numeric vector.")
        if (any(is.na(Y.test)))
            stop("'Y.test' cannot contain missing values.")
    }

    if (!is.matrix(X.train) || !is.numeric(X.train))
        stop("'X.train' must be a numeric vector.")
    if (nrow(X.train) != length(Y.train))
        stop("The number of rows of 'X.train' does not match the length of 'Y.train'.")
    if (any(is.na(X.train)))
        stop("'X.train' cannot contain missing values.")

    if (!is.null(X.test)) {
        if (!is.matrix(X.test) || !is.numeric(X.test))
            stop("'X.test' must be a numeric vector.")
        if (nrow(X.test) != length(Y.test))
            stop("The number of rows of 'X.test' does not match the length of 'Y.test'.")
        if (ncol(X.test) != ncol(X.train))
            stop("'X.test' does not have the same number of columns as 'X.train'.")
        if (any(is.na(X.test)))
            stop("'X.test' cannot contain missing values.")
    }

    if (!is.matrix(Z.train) || !is.numeric(Z.train))
        stop("'Z.train' must be a numeric vector.")
    if (nrow(Z.train) != length(Y.train))
        stop("The number of rows of 'Z.train' does not match the length of 'Y.train'.")
    if (any(is.na(Z.train)))
        stop("'Z.train' cannot contain missing values.")

    if (!is.null(Z.test)) {
        if (!is.matrix(Z.test) || !is.numeric(Z.test))
            stop("'Z.test' must be a numeric vector.")
        if (nrow(Z.test) != length(Y.test))
            stop("The number of rows of 'Z.test' does not match the length of 'Y.test'.")
        if (ncol(Z.test) != ncol(Z.train))
            stop("'Z.test' does not have the same number of columns as 'Z.train'.")
        if (any(is.na(Z.test)))
            stop("'Z.test' cannot contain missing values.")
    }

    if (!is.numeric(sigma) || sigma < 0)
        stop("'sigma' must be a nonnegative number.")
    if (length(sigma) > 1)
        stop("'sigma' must have unit length.")

    if (!is.numeric(degree) || degree < 1)
        stop("'degree' should be an integer >= 1.")

    if (!is.numeric(maxit) || maxit < 1)
        stop("'maxit' should be an integer >= 1.")

    if (!is.numeric(delta) || delta <= 0)
        stop("'delta' should be a postive number.")

    generate.Xm <- function(i)
    {
        m <- splines::bs(c(X.train[, i], X.test[, i]), degree = degree)
        m <- qr.Q(qr(m))
        apply(m, 2, function(x) x / stats::sd(x))
    }

    Xm <- lapply(1:ncol(X.train), generate.Xm)

    if (is.null(colnames(X.train)))
        colnames(X.train) <- 1:ncol(X.train)
    names(Xm) <- colnames(X.train)

    if (is.null(names(Xm)))
        names(Xm) <- paste0("G", 1:length(Xm))

    Xm.train <- lapply(Xm, function(x) x[1:nrow(X.train), ])
    Xm.test  <- lapply(Xm, function(x) x[-(1:nrow(X.train)), ])
    Xi.train <- generate_Xi(Xm.train)
    Xi.test <- generate_Xi(Xm.test)

    X.init <- do.call("cbind", Xm.train)

    n.main <- ncol(X.init)

    higlasso.groups <- lapply(1:length(Xm.train), function(i) rep(i, ncol(Xm.train[[i]])))
    n.groups <- length(Xm.train)
    n <- length(Xm.train)
    for (j in 1:n.groups) {
        for (i in 1:n.groups) {
            p <- ncol(Xi.train[[i, j]])
            if (p > 0) {
                X.init <- cbind(X.init, Xi.train[[i, j]])
                n <- n + 1
                higlasso.groups[[n]] <- rep(n, p)
            }
        }
    }

    # generate lambda1/2 sequences if user does not pre-specify them
    if (!is.null(lambda1)) {
        if (!is.numeric(lambda1) || any(lambda1 <= 0))
            stop("'lambda1' must be a nonnegative numeric array.")
    } else {
        lambda1.max <- max(abs(Y.train %*% X.init[, 1:n.main]) / nrow(X.init))
        lambda1.min <- lambda.min.ratio * lambda1.max
        lambda1 <- exp(seq(log(lambda1.max), log(lambda1.min), length.out =
                           n.lambda1))
        # lambda1 <- seq(lambda1.max, lambda1.min, length.out = n.lambda1)
    }

    if (!is.null(lambda2)) {
        if (!is.numeric(lambda2) || any(lambda2 <= 0))
            stop("'lambda2' must be a nonnegative numeric array.")
    } else {
        lambda2.max <- max(abs(Y.train %*% X.init[, -(1:n.main)]) / nrow(X.init))
        lambda2.min <- lambda.min.ratio * lambda2.max
        lambda2 <- exp(seq(log(lambda2.max), log(lambda2.min),
                           length.out = n.lambda2))
        # lambda2 <- seq(lambda2.max, lambda2.min, length.out = n.lambda2)
    }

    nx  <- ncol(X.init)
    X.init  <- cbind(X.init, Z.train)
    weights <- c(rep(1, nx), rep(0, ncol(Z.train)))
    out <- purrr::map(purrr::cross2(lambda1, lambda2), function(lambda)
    {
        e.net <- gcdnet::gcdnet(X.init, Y.train, method = "ls", lambda2 =
                                lambda[[2]], pf = weights, pf2 = weights,
                                eps = delta, maxit = max(maxit, 1e6))

        if (e.net$jerr != 0)
            stop("Error in gcdnet::gcdnet.")

        # get the best scoring lambda from gcdnet and use that to generate
        # inital weights for the adpative elastic net
        mse <- function(p) mean((p - Y.train) ^ 2)
        i <- which.min(apply(stats::predict(e.net, X.init), 2, mse))

        ae.weights <- 1 / (e.net$beta[1:nx, i] + 1 / nrow(X.init))
        ae.weights <- c(ae.weights, rep(0, ncol(Z.train)))
        ae.net <- gcdnet::gcdnet(X.init, Y.train, method = "ls", lambda =
                                 lambda[[1]], lambda2 = lambda[[2]], pf =
                                 ae.weights, eps = delta, maxit = max(maxit, 1e6))

        if (e.net$jerr != 0)
            stop("Error in gcdnet::gcdnet.")

        coefs <- ae.net$beta[1:nx]

        n <- 1
        higlasso.coefs <- lapply(1:length(higlasso.groups), function(i)
        {
            j <- length(higlasso.groups[[i]])
            ret <- coefs[n:(n + j - 1)]
            n <<- n + j
            ret
        })
        beta <- higlasso.coefs[1:length(Xm.train)]
        eta  <- higlasso.coefs[-(1:length(Xm))]
        higlasso.out <- higlasso_internal(Y.train, Xm.train, Xi.train, Z.train,
                                          beta, eta, lambda[[1]], lambda[[2]],
                                          sigma, maxit, delta)

        names(higlasso.out$beta) <- names(Xm)

        higlasso.out$degree <- degree
        higlasso.out$lambda <- c(lambda[[1]], lambda[[2]])

        class(higlasso.out) <- "higlasso"
        higlasso.out
    })
    if (!is.null(Y.test) && !is.null(X.test) && !is.null(Z.test)) {
        predictions <- purrr::map(out, predict, newdata = list(Xm.test, Z.test))
        mse <- purrr::map_dbl(predictions, ~ mean((.x - Y.test)^2))

        purrr::map2(mse, out, ~ list(.x, .y))
    } else {
        out
    }
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
    n <- length(object$beta)
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
    n <- length(object$beta)
    beta <- object$beta
    eta  <- object$eta

    if (!missing(newdata)) {
        if (!is.list(newdata) || length(newdata) != 2)
            stop("'newdata' should be a length 3 list.")
        Xm <- newdata[[1]]
        Xi <- generate_Xi(Xm)
        Z  <- newdata[[2]]
    }
    else {
        Xm  <- object$Xm
        Xi  <- object$Xi
        Z <- object$Z
    }
    Y.hat  <- Z %*% object$alpha
    for (i in 1:n)
        Y.hat <- Y.hat + (Xm[[i]] %*% beta[[i]])

    for (j in 1:n)
        for (i in 1:n) {
            if (nrow(eta[[i, j]]) > 0) {
                e <- eta[[i, j]] * kronecker(beta[[i]], beta[[j]])
                Y.hat <- Y.hat + Xi[[i, j]] %*% e
            }
        }
    Y.hat
}
