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
#' @param Y A length n numeric response vector
#' @param X A n x p numeric matrix
#' @param Z A n x m numeric matrix
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
#' @param nfolds Number of folds for cross validation. Default is 10. The
#'     minimum is 3, and while the maximum is the number of observations
#'     (ie leave one out cross validation), this is a bad idea.
#' @param foldid TODO
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
#' X <- as.matrix(X[1:400,])
#' Z.train <- matrix(1, 400)
#'
#' X.test <- as.matrix(X[401:500,])
#' Y.test  <- Y[401:500]
#' Z.test  <- matrix(1, 100)
#' \dontrun{
#' # This can take a bit of time
#' higlasso.out <- higlasso(Y.train, X, Z.train, Y.test = Y.test,
#'                          X.test = X.test, Z.test = Z.test)
#' }
#' @export
cv.higlasso <- function(Y, X, Z, method = "gglasso" ,lambda1 = NULL, lambda2 = NULL,
                        n.lambda1 = 10, n.lambda2 = 10, lambda.min.ratio = .1,
                        nfolds = 10, foldid = NULL, sigma = 1, degree = 3,
                        maxit = 5000, delta = 1e-5)
{

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

    fit <- higlasso(Y, X, Z, method = method, lambda1 = lambda1, lambda2 =
                    lambda2, n.lambda1 = n.lambda1, n.lambda2 = n.lambda2,
                    lambda.min.ratio = lambda.min.ratio, sigma = sigma,
                    degree = degree, maxit = maxit, delta = delta)

    n <- length(Y)
    if (!is.null(foldid)) {
        stop("Not implemented")
    } else {
      r     <- n %% nfolds
      p     <- (n - r) / nfolds
      folds <- c(rep(1:nfolds, p), 1:r)
      folds <- sample(folds, n)
    }

    lambda1   <- unique(fit$lambda[, 1])
    lambda2   <- unique(fit$lambda[, 2])
    cv.models <- vector("list", nfolds)
    for (i in 1:nfolds) {
        Y.train <- Y[folds != i]
        Y.test  <- Y[folds == i]
        X.train <- X[folds != i, , drop = F]
        X.test  <- X[folds == i, , drop = F]
        Z.train <- Z[folds != i, , drop = F]
        Z.test  <- Z[folds == i, , drop = F]

        cv.models[[i]] <- higlasso(Y.train, X.train, Z.train, method = method,
                                   Y.test = Y.test, X.test = X.test,
                                   Z.test = Z.test, lambda1 = lambda1,
                                   lambda2 = lambda2, n.lambda1 = n.lambda1,
                                   n.lambda2 = n.lambda2, lambda.min.ratio =
                                   lambda.min.ratio, sigma = sigma,
                                   degree = degree, maxit = maxit,
                                   delta = delta)
    }

    cvm  <- purrr::reduce(purrr::map(cv.models, ~ .x$mse.test), `+`) / nfolds
    cvse <- purrr::reduce(purrr::map(cv.models, ~ .x$mse.test),~ .x + (.y - cvm) ^ 2)
    cvse <- sqrt(cvse / nfolds - 1) / sqrt(nfolds)

    fit$cvse <- cvse
    fit$cvm  <- cvm
    structure(list(cvm = cvm, cvse = cvse, lambda1 = lambda1, lambda2 = lambda2,
                   higlasso.fit = fit), class = "cv.higlasso")
}
