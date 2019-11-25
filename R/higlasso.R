#' Hierarchical Integrative Group LASSO
#'
#' HiGLASSO is a regularization method designed to detect non linear
#' interactions between variables, particulary exposures in environmental
#' health studies.
#' We have designed \code{higlasso} to
#' \itemize{
#'   \item Impose strong heredity constraints on two-way interaction effects
#'       (hierarchical).
#'   \item Incorporate adaptive weights without necessitating initial
#'       coefficient estimates.
#'   \item Induce sparsity for variable selection while respecting group
#'       structure (group LASSO).
#' }
#'
#' The objective function \code{higlasso} solves is
#' % Renders the objective function as html in Rstudio and in raw tex otherwise.
#' \ifelse{html}{\out{<center> argmin &beta;<sub>j</sub>, &eta;<sub>jj'</sub>
#' &frac12;|| Y - X<sub>j</sub> &beta;<sub>j</sub> -
#' X<sub>jj'</sub> (&eta;<sub>jj'</sub> &odot; &beta;<sub>j</sub>
#' &otimes; &beta;<sub>j'</sub>)||<sup>2</sup>
#' + &lambda;<sub>1</sub>w<sub>j</sub>||&beta;<sub>j</sub> || +
#' &lambda;<sub>2</sub> w<sub>jj'</sub> ||&eta;<sub>jj'</sub>||
#' </center>}}{
#' \deqn{argmin \\beta_j, \\eta_{jj'} \frac{1}{2}|| Y - X_j \\beta_j}
#' \deqn{- X_{jj'} (\\eta_{jj'} \odot \\beta_j \otimes \\beta_{j'})||^2}
#' \deqn{+ \\lambda_1 w_j ||\\beta_j|| + \\lambda_2 w_{jj'} || \\eta_{jj'}||}}
#' @param Y.train A length n numeric response vector
#' @param X.train A n x p numeric matrix
#' @param Z.train A n x m numeric matrix
#' @param Y.test A length n' numeric response vector
#' @param X.test A n' x p numeric matrix
#' @param Z.test A n' x m numeric matrix
#' @param lambda1 A numeric vector of main effect penalties on which to tune
#'     By default, \code{lambda1 = NULL} and generates a sequence (length
#'     \code{n.lambda1}) of lambda1s based off of the data and
#'     \code{min.lambda.ratio}
#' @param lambda2 A numeric vector of interaction effects penalties on which to
#'     tune. By default, \code{lambda2 = NULL} and generates a sequence (length
#'     \code{n.lambda2}) of lambda2s based off of the data and
#'     \code{min.lambda.ratio}
#' @param n.lambda1 The number of lambda1 values to generate. Default is 10,
#'     minimum is 2. If \code{lambda1 != NULL}, this parameter is ignored
#' @param n.lambda2 The number of lambda2 values to generate. Default is 10,
#'     minimum is 2. If \code{lambda2 != NULL}, this parameter is ignored
#' @param lambda.min.ratio Ratio that calculates min lambda from max lambda
#' @param sigma Scale parameter for integrative weights. Technically a third
#'     tuning parameter but defaults to 1 for computational tractibility
#' @param degree Degree of \code{bs} basis expansion. Default is 3
#' @param maxit Maximum number of iterations. Default is 5000
#' @param delta Tolerance for convergence. Defaults to 1e-5
#' @author Alexander Rix
#' @references TODO
#' @return TODO
#' @examples
#' library(higlasso)
#'
#' X <- higlasso.df[, paste0("X", 1:10)]
#' Y <- higlasso.df$y
#'
#' Y.train <- Y[1:400]
#' X.train <- as.matrix(X[1:400,])
#' Z.train <- matrix(1, 400)
#'
#' X.test <- as.matrix(X[401:500,])
#' Y.test  <- Y[401:500]
#' Z.test  <- matrix(1, 100)
#' \dontrun{
#' # This can take a bit of time
#' higlasso.out <- higlasso(Y.train, X.train, Z.train, Y.test = Y.test,
#'                          X.test = X.test, Z.test = Z.test)
#' }
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
        stop("The number of rows of 'X.train' does not match the length of ",
             "'Y.train'.")
    if (any(is.na(X.train)))
        stop("'X.train' cannot contain missing values.")

    if (!is.null(X.test)) {
        if (!is.matrix(X.test) || !is.numeric(X.test))
            stop("'X.test' must be a numeric vector.")
        if (nrow(X.test) != length(Y.test))
            stop("The number of rows of 'X.test' does not match the length of ",
                 "'Y.test'.")
        if (ncol(X.test) != ncol(X.train))
            stop("'X.test' does not have the same number of columns as ",
                       "'X.train'.")
        if (any(is.na(X.test)))
            stop("'X.test' cannot contain missing values.")
    }

    if (!is.matrix(Z.train) || !is.numeric(Z.train))
        stop("'Z.train' must be a numeric vector.")
    if (nrow(Z.train) != length(Y.train))
        stop(paste("The number of rows of 'Z.train' does not match the length",
                   "of 'Y.train'."))
    if (any(is.na(Z.train)))
        stop("'Z.train' cannot contain missing values.")

    if (!is.null(Z.test)) {
        if (!is.matrix(Z.test) || !is.numeric(Z.test))
            stop("'Z.test' must be a numeric vector.")
        if (nrow(Z.test) != length(Y.test))
            stop("The number of rows of 'Z.test' does not match the length of ",
                 "'Y.test'.")
        if (ncol(Z.test) != ncol(Z.train))
            stop("'Z.test' does not have the same number of columns as ",
                 "'Z.train'.")
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
    Xm <- purrr::map(1:ncol(X.train), generate.Xm)

    # get number of main effect variables.
    p.main <- sum(purrr::map_dbl(Xm, function(Xj) ncol(Xj)))

    Xm.train <- purrr::map(Xm, function(Xj) Xj[1:nrow(X.train), ])
    Xm.test  <- purrr::map(Xm, function(Xj) Xj[-(1:nrow(X.train)), ])
    Xi.train <- generate_Xi(Xm.train)

    j <- purrr::map_lgl(Xi.train, ~ ncol(.x) > 0)
    n <- length(Xm.train)
    groups <- purrr::flatten_dbl(c(
        purrr::imap(Xm.train,    function(Xm.i, i) rep(i, ncol(Xm.i))),
        purrr::imap(Xi.train[j], function(Xi.i, i) rep(n + i, ncol(Xi.i)))
    ))

    # construct "inverse" of groups
    i.groups <- vector("list", max(groups))
    purrr::iwalk(groups, function(g, i) i.groups[[g]] <<- c(i.groups[[g]], i))

    X.init <- do.call("cbind", c(Xm.train, Xi.train[j]))

    # generate lambda sequences if user does not pre-specify them
    YtX <- abs(Y.train %*% X.init)[1,] / nrow(X.init)
    if (!is.null(lambda1)) {
        if (!is.numeric(lambda1) || any(lambda1 <= 0))
            stop("'lambda1' must be a nonnegative numeric array.")
    } else {
        lambda1.max <- max(YtX[1:p.main])
        lambda1.min <- lambda.min.ratio * lambda1.max
        lambda1 <- exp(seq(log(lambda1.max), log(lambda1.min), length.out =
                           n.lambda1))
    }

    if (!is.null(lambda2)) {
        if (!is.numeric(lambda2) || any(lambda2 <= 0))
            stop("'lambda2' must be a nonnegative numeric array.")
    } else {
        lambda2.max <- max(YtX[-(1:p.main)])
        lambda2.min <- lambda.min.ratio * lambda2.max
        lambda2 <- exp(seq(log(lambda2.max), log(lambda2.min), len = n.lambda2))
    }

    p       <- ncol(X.init)
    X.init  <- cbind(X.init, Z.train)
    weights <- c(rep(1, p), rep(0, ncol(Z.train)))

    models <- purrr::map(purrr::cross2(lambda1, lambda2), function(lambda)
    {
        e.net <- gcdnet::gcdnet(X.init, Y.train, method = "ls", lambda2 =
                                lambda[[2]], pf = weights, pf2 = weights,
                                eps = delta, maxit = max(maxit, 1e6))

        if (e.net$jerr != 0)
            stop("Error in gcdnet::gcdnet.")

        # get the best scoring lambda from gcdnet and use that to generate
        # inital weights for the adpative elastic net
        mse <- function(Y.hat) mean((Y.hat - Y.train) ^ 2)
        i <- which.min(apply(stats::predict(e.net, X.init), 2, mse))

        ae.weights <- sqrt(abs(1 / (e.net$beta[1:p, i] + 1 / nrow(X.init))))
        ae.weights <- c(ae.weights, rep(0, ncol(Z.train)))
        ae.net     <- gcdnet::gcdnet(X.init, Y.train, method = "ls",
                                     lambda2 = lambda[[2]], pf = ae.weights,
                                     eps = delta, maxit = max(maxit, 1e6))

        if (e.net$jerr != 0)
            stop("Error in gcdnet::gcdnet.")
        i <- which.min(apply(stats::predict(ae.net, X.init), 2, mse))

        coefs <- purrr::map(i.groups, ~ ae.net$beta[.x, i])
        beta <- coefs[1:n]
        eta  <- coefs[-(1:n)]
        out <- higlasso_internal(Y.train, Xm.train, Xi.train, Z.train, beta,
                                 eta, lambda[[1]], lambda[[2]], sigma, maxit,
                                 delta)

        if (is.null(colnames(X.train)))
            names(out$beta) <- paste0("V", 1:length(Xm))
        else
            names(out$beta) <- colnames(X.train)

        out$degree <- degree
        out$lambda <- c(lambda[[1]], lambda[[2]])
        out$df = sum(purrr::map_lgl(c(out$beta, out$eta[j]), ~ any(.x != 0)))
        class(out) <- "higlasso"
        out
    })
    if (!is.null(Y.test) && !is.null(X.test) && !is.null(Z.test)) {
        newdata <- list(Xm.test, Z.test)
        predictions <- purrr::map(models, stats::predict, newdata = newdata)
        mse.test <- purrr::map_dbl(predictions, ~ mean((.x - Y.test)^2))

    } else {
        mse.test <- rep(NA, length(models))
    }

    out <- purrr::reduce(models, .init = list(lambda = NULL, mse.train = NULL,
                         mse.test = mse.test),
                         function(list, model)
                         {
                             list(lambda = rbind(list$lambda, model$lambda),
                                  mse.train = c(list$mse.train, model$mse),
                                  mse.test = list$mse.test)
                         }
    )
    out$model  <- models
    class(out) <- "higlasso.grid"
    out
}

#' Print higlasso fits
#' @param x An object of type 'higlasso'
#' @param ... Additional parameters to pass onto print
#' @export
print.higlasso.grid <- function(x, ...)
{
    if (class(x) != "higlasso.grid")
        stop("'x' is not a higlasso fit.")


    df <-data.frame(lambda1 = x$lambda[,1], lambda2 = x$lambda[, 2],
                    mse.train = x$mse.train, mse.test = x$mse.test,
                    df = purrr::map_dbl(x$model, ~ .x$df))

    print(df, zero.print = "", right = F)
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
    cat(sprintf("Mean squared prediction error: %f\n", sum$mse))
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
