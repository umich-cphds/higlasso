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
higlasso <- function(Y.train, X.train, Z.train, method = "gglasso", Y.test = NULL,
                     X.test = NULL, Z.test = NULL, lambda1 = NULL, lambda2 = NULL,
                     n.lambda1 = 10, n.lambda2 = 10, lambda.min.ratio = .1,
                     sigma = 1, degree = 3, maxit = 5000, delta = 1e-5)
{
    check.Y(Y.train)
    if (!is.null(Y.test))
        check.Y(Y.test)

    check.XZ(X.train, Y.train)
    if (!is.null(X.test)) {
        check.XZ(X.test, Y.test)
        if (ncol(X.test) != ncol(X.train))
            stop("'X.test' does not have the same number of columns as ",
                 "'X.train'.")
    }

    check.XZ(Z.train, Y.train)
    if (!is.null(Z.test)) {
        check.XZ(Z.test, Y.test)
        if (ncol(Z.test) != ncol(Z.train))
            stop("'Z.test' does not have the same number of columns as ",
                 "'Z.train'.")
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

    # get number of main effect variables.

    matrices <- generate_design_matrices(rbind(X.train, X.test), degree)

    Xm <- matrices$Xm
    Xi <- matrices$Xi
    p.main <- sum(purrr::map_dbl(Xm, function(Xj) ncol(Xj)))
    j <- purrr::map_lgl(Xi, ~ ncol(.x) > 0)

    Xm.train <- purrr::map(Xm, function(Xj) Xj[1:nrow(X.train), ])
    Xm.test  <- purrr::map(Xm, function(Xj) Xj[-(1:nrow(X.train)), ])

    Xi.train <- lapply(Xi, function(x)
    {
        if (ncol(x) > 0)
            x[1:nrow(X.train),]
        else
            x
    })
    Xi.test <- lapply(Xi, function(x)
    {
        if (ncol(x) > 0)
            x[-(1:nrow(X.train)),]
        else
            x
    })

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

    px       <- ncol(X.init)
    pz       <- ncol(Z.train)
    X.init  <- cbind(X.init, Z.train)

    if (method == "gglasso")
        coefs <- initialise_higlasso(method, X.init, p, ncol(Z.train), Y.train,
                                     lambda, delta, maxit, groups, i.groups)

    models <- purrr::map(purrr::cross2(lambda1, lambda2), function(lambda)
    {
        if (method == "aenet")
            coefs <- initialise_higlasso(method, X.init, px, pz, Y.train,
                                         lambda, delta, maxit, groups, i.groups)

        beta  <- coefs[1:n]
        eta   <- coefs[-(1:n)]

        higlasso.fit(beta, eta, j, Y.train, Xm.train, Xi.train, Z.train,
                     lambda[[1]], lambda[[2]], sigma, maxit, delta, degree, X.train)
    })

    # Calculate test error if given.
    if (!is.null(Y.test) && !is.null(X.test) && !is.null(Z.test)) {
        newdata     <- list(Xm.test, Xi.test, Z.test)
        predictions <- purrr::map(models, stats::predict, newdata = newdata)
        mse.test    <- purrr::map_dbl(predictions, ~ mean((.x - Y.test) ^ 2))
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

# Type checking functions to save space.
check.Y <- function(Y)
{
    name <- deparse(substitute(Y))
    if (!is.vector(Y) || !is.numeric(Y))
        stop("'", name ,"' must be a numeric vector.")
        if (any(is.na(Y)))
        stop("'", name, "' cannot contain missing values.")
}

check.XZ <- function(XZ, Y)
{
    name.XZ <- deparse(substitute(XZ))
    name.Y <- deparse(substitute(Y))
    if (!is.matrix(XZ) || !is.numeric(XZ))
        stop("'", name.XZ, "' must be a numeric matrix.")
    if (nrow(XZ) != length(Y))
        stop("The number of rows of '", name.XZ, "' does not match the length",
             " of '", name.Y, "'.")
    if (any(is.na(XZ)))
        stop("'", name.XZ, "' cannot contain missing values.")

}

initialise_higlasso <- function(method, X.init, px, pz, Y.train, lambda, delta,
                                maxit, groups, i.groups)
{

    if (method == "aenet") {
        # penalty factor for enet. Z contains unregularized coefficents so we set
        # those weights to 0
        pf <- c(rep(1, px), rep(0, pz))

        enet <- gcdnet::cv.gcdnet(X.init, Y.train, method = "ls", lambda2 =
        lambda[[2]], pf = pf, pf2 = pf, eps = delta,
        maxit = max(maxit, 1e6))

        # get the best scoring lambda from gcdnet and use that to generate
        # inital weights for the adpative elastic net
        i <- which.min(enet$cvm)
        weights <- enet$gcdnet.fit$beta[1:px, i]
        weights <- 1 / abs(weights + 1 / nrow(X.init)) ^ 2
        weights <- c(weights, rep(0, pz))
        aenet <- gcdnet::cv.gcdnet(X.init, Y.train, method = "ls", lambda2 =
        lambda[[2]], pf = weights, pf2 = pf,
        eps = delta, maxit = max(maxit, 1e6))

        i <- which.min(aenet$cvm)
        purrr::map(i.groups, ~ aenet$gcdnet.fit$beta[.x, i])
    } else {
        groups <- c(groups, seq(pz) + max(groups))
        fit <- gglasso::cv.gglasso(X.init, Y.train, group = groups)
        i   <- which.min(fit$cvm)
        purrr::map(i.groups, ~ fit$gglasso.fit$beta[.x, i])
    }
}



higlasso.fit <- function(beta, eta, j, Y, Xm, Xi, Z, lambda1, lambda2, sigma,
                         maxit, delta, degree, X)
{
    out <- higlasso_internal(Y, Xm, Xi, Z, beta, eta, lambda1, lambda2, sigma,
                             maxit, delta)

    if (is.null(colnames(X)))
        names(out$beta) <- paste0("V", 1:length(Xm))
    else
        names(out$beta) <- colnames(X)

    out$lambda <- c(lambda1, lambda2)
    out$df = sum(purrr::map_lgl(c(out$beta, out$eta[j]), ~ any(.x != 0)))
    out$init <- unlist(c(beta, eta))
    out$degree <- degree

    class(out) <- "higlasso.fit"

    out
}
