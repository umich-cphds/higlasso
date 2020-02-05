#' Cross Validated Hierarchical Integrative Group LASSO
#'
#' HiGLASSO is a regularization based selection method designed to detect
#' non-linear interactions between variables, particulary exposures in
#' environmental health studies.
#' We have designed HiGLASSO to
#' \itemize{
#'   \item Impose strong heredity constraints on two-way interaction effects
#'       (hierarchical).
#'   \item Incorporate adaptive weights without necessitating initial
#'       coefficient estimates (integrative).
#'   \item Induce sparsity for variable selection while respecting group
#'       structure (group LASSO).
#' }
#'
#' The objective function \code{higlasso} solves is
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
#'
#' @param Y A length n numeric response vector
#' @param X A n x p numeric matrix
#' @param Z A n x m numeric matrix
#' @param method Type of initialization to use. Possible choices are "gglasso"
#'     for group LASSO and "aenet" for adaptive elastic net. Default is "aenet"
#' @param lambda1 A numeric vector of main effect penalties on which to tune
#'     By default, \code{lambda1 = NULL} and higlasso generates a length
#'     \code{nlambda1} sequence of lambda1s based off of the data and
#'     \code{min.lambda.ratio}
#' @param lambda2 A numeric vector of interaction effects penalties on which to
#'     tune. By default, \code{lambda2 = NULL} and generates a sequence (length
#'     \code{nlambda2}) of lambda2s based off of the data and
#'     \code{min.lambda.ratio}
#' @param nlambda1 The number of lambda1 values to generate. Default is 10,
#'     minimum is 2. If \code{lambda1 != NULL}, this parameter is ignored
#' @param nlambda2 The number of lambda2 values to generate. Default is 10,
#'     minimum is 2. If \code{lambda2 != NULL}, this parameter is ignored
#' @param lambda.min.ratio Ratio that calculates min lambda from max lambda.
#'     Ignored if 'lambda1' or 'lambda2' is non NULL. Default is 0.05
#' @param nfolds Number of folds for cross validation. Default is 10. The
#'     minimum is 3, and while the maximum is the number of observations
#'     (ie leave one out cross validation)
#' @param foldid TODO
#' @param sigma Scale parameter for integrative weights. Technically a third
#'     tuning parameter but defaults to 1 for computational tractibility
#' @param degree Degree of \code{bs} basis expansion. Default is 3
#' @param maxit Maximum number of iterations. Default is 5000
#' @param tol Tolerance for convergence. Defaults to 1e-5
#' @author Alexander Rix
#' @references TODO
#' @return
#' An object of type "cv.higlasso" with 7 elements
#' \describe{
#' \item{lambda}{An \code{nlambda1 x nlambda2 x 2} array containing each
#'     pair \code{(lambda1, lambda2)} pair.}
#' \item{lambda.min}{lambda pair with the lowest cross validation error}
#' \item{lambda.1se}{}
#' \item{cvm}{cross validation error at each lambda pair. The error is
#'     calculated from the mean square error.}
#' \item{cvse}{standard error of 'cvm' at each lambda pair.}
#' \item{higlasso.fit}{higlasso output from fitting the whole data.}
#' \item{call}{The call that generated the output.}
#' }
#' @examples
#' library(higlasso)
#'
#' X <- as.matrix(higlasso.df[, paste0("V", 1:10)])
#' Y <- higlasso.df$Y
#' Z <- matrix(1, nrow(X))
#'
#' \dontrun{
#' # This can take a bit of time
#' cv.fit <- cv.higlasso(Y, X, Z)
#' }
#' @export
cv.higlasso <- function(Y, X, Z, method = c("aenet", "gglasso"), lambda1 = NULL,
                        lambda2 = NULL, nlambda1 = 10, nlambda2 = 10,
                        lambda.min.ratio = .05, nfolds = 10, foldid = NULL,
                        sigma = 1, degree = 3, maxit = 5000, tol = 1e-5)
{
    call <- match.call()
    method <- match.arg(method)
    fit <- higlasso(Y, X, Z, method, lambda1, lambda2, nlambda1, nlambda2,
                    lambda.min.ratio, sigma, degree, maxit, tol)

    lambda1 <- fit$lambda[, 1, 1]
    lambda2 <- fit$lambda[1, , 2]

    nlambda1 <- length(lambda1)
    nlambda2 <- length(lambda2)

    n <- length(Y)
    if (!is.null(foldid)) {
        stop("Not implemented")
    } else {
      r     <- n %% nfolds
      p     <- (n - r) / nfolds
      folds <- c(rep(1:nfolds, p), seq(len = r))
      folds <- sample(folds, n)
    }

    matrices <- generate_design_matrices(X, degree)

    Xm <- matrices$Xm
    Xi <- matrices$Xi
    X.xp <- matrices$X.xp
    groups <- matrices$groups
    igroups <- matrices$igroups

    px       <- ncol(X.xp)
    pz       <- ncol(Z)
    X.xp  <- cbind(X.xp, Z)

    cvm <- array(0, c(nlambda1, nlambda2, nfolds))
    cvse <- matrix(0, nlambda1, nlambda2)
    for (i in 1:nfolds) {
        Y.train <- Y[folds != i]
        Z.train <- Z[folds != i,, drop = F]

        Xm.train <- purrr::map(Xm, ~ .x[folds != i,, drop = F])
        Xi.train <- purrr::map(Xi, function(Xi)
            if (nrow(Xi) > 0)
                Xi[folds != i,, drop = F]
            else
                Xi
        )
        X.xp.train <- X.xp[folds != i,, drop = F]

        X.xp.test <- X.xp[folds == i,, drop = F]
        Y.test  <- Y[folds == i]

        cv.fit <- higlasso.fit(Y.train, Xm.train, Xi.train, Z.train, X.xp.train,
                               px, pz, method, lambda1,  lambda2, sigma, groups,
                               igroups, maxit, tol, call)

        for (j in seq(nlambda2)) {
            for (k in seq(nlambda1)) {
                res <- Y.test - X.xp.test %*% cv.fit$coef[j, k, ]
                cvm[j, k, i] <- mean(res ^ 2)
            }
        }

    }

    cvse <- apply(cvm, c(1, 2), function(x) stats::sd(x) / sqrt(nfolds))
    cvm  <- apply(cvm, c(1, 2), mean)

    i <- which.min(cvm)

    lambda.min <- fit$lambda[c(i, i + nlambda1 * nlambda2)]


    j <- abs(cvm - min(cvm)) < cvse[i]
    # Inf could cause NaN if df = 0
    i <- which.min(fit$df * ifelse(j, 1, 1e9))

    lambda.1se <- fit$lambda[c(i, i + nlambda1 * nlambda2)]
    structure(list(lambda = fit$lambda, lambda.min = lambda.min,
                   lambda.1se = lambda.1se, cvm = cvm, cvse = cvse,
                   higlasso.fit = fit, call = call), class = "cv.higlasso")
}
