generate_design_matrices <- function(X, degree)
{
    generate.Xm <- function(i)
    {
        m <- splines::bs(X[, i], degree = degree)
        apply(m, 2, function(x) (x - mean(x)) / stats::sd(x))
    }

    Xm <- purrr::map(1:ncol(X), generate.Xm)
    Xi <- generate_Xi(Xm)
    decomp <- function(Xmi)
    {
        if (ncol(Xmi) > 0)
            apply(qr.Q(qr(Xmi)), 2, function(x) x / stats::sd(x))
        else
            Xmi
    }

    list(Xm = purrr::map(Xm, decomp), Xi = purrr::map(Xi, decomp))
}


#' Print higlasso fits
#' @param x An object of type 'higlasso'
#' @param ... Additional parameters to pass onto print
#' @export
print.higlasso.grid <- function(x, ...)
{
    if (class(x) != "higlasso.grid")
        stop("'x' is not a higlasso fit.")


    df <- data.frame(lambda1 = x$lambda[,1], lambda2 = x$lambda[, 2],
                     mse.train = x$mse.train, mse.test = x$mse.test,
                     df = purrr::map_dbl(x$model, ~ .x$df))

    print(df, zero.print = "", right = F)
}

#' Print higlasso fits
#' @param x An object of type 'higlasso'
#' @param ... Additional parameters to pass onto print
#' @export
print.higlasso.fit <- function(x, ...)
{
    if (class(x) != "higlasso.fit")
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
    if (class(object) != "higlasso.fit")
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
predict.higlasso.fit <- function(object, newdata, ...)
{
    if (class(object) != "higlasso.fit")
        stop("'object' is not a higlasso fit.")
    n <- length(object$beta)
    beta <- object$beta
    eta  <- object$eta

    if (!missing(newdata)) {
        if (!is.list(newdata) || length(newdata) != 3)
            stop("'newdata' should be a length 3 list.")
        Xm <- newdata[[1]]
        Xi <- newdata[[2]]
        dim(Xi) <- c(n, n)
        Z  <- newdata[[3]]
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
