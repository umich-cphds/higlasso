require(MASS)

## transform theta vector to theta list
theta.to.list <- function(theta, groupsizes)
{
  n.groups <- length(groupsizes)
  theta.list <- vector("list", S)
  start <- 1
  for (i in 1:n.groups) {
    theta.list[[i]] <- theta[start:(start + groupsizes[i] - 1)]
    start <- start + groupsizes[i]
  }
  theta.list
}

## transform eta/gamma vector to eta/gamma list
eta.to.list <- function(eta, groupsizes)
{
n <- length(groupsizes)
eta.list <- vector("list", n * (n - 1) / 2)
start <- 1
ind   <- 1
for (i in 1:(n - 1)) {
for (j in (i + 1):n) {
  size <- groupsizes[i] * groupsizes[j]
  eta.list[[ind]] <- eta[start:(start + size - 1)]
  start <- start + size
  ind <- ind + 1
}
}
eta.list
}

## transform theta list and eta to gamma
eta.to.gamma <- function(theta.list, eta, groupsizes)
{
  n <- length(groupsizes)
  gamma <- rep(0, length(eta))
  gamma.list <- vector("list", n)
  ind <- 1
  start <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
	  size <- groupsizes[i] * groupsizes[j]
	  range <- start:(start + size - 1)
	  gamma[range] <- (theta.list[[i]] %x% theta.list[[j]]) * eta[range]
      gamma.list[[ind]] <- gamma[range]
	  start <- start + size
      ind <- ind + 1
	}
  }
  list(gamma = gamma, gamma.list = gamma.list)
}

## compute the penalized likelihood value
penlik <- function(Y, Xm, Xi, Z, theta, eta, alpha, theta.list, eta.list, gamma,
				     groupsizes, lambdas, sigmas, n)
{
  if (is.null(theta.list))
    theta.list <- theta.to.list(theta, groupsize)
  if (is.null(eta.list))
    eta.list <- eta.to.list(eta, groupsize)
  if (is.null(gamma))
    gamma <- eta.to.gamma(theta.list, eta, groupsize)$gamma
  resi <- Y - Z %*% alpha - Xm %*% theta - Xi %*% gamma

  lik <- as.numeric(crossprod(resi) * 0.5) / n
  theta.penalty <- lambdas[1] * pen(theta.list, sigmas[1])
  eta.penalty   <- lambdas[2] * pen(eta.list, sigmas[2])
  lik + theta.penalty + eta.penalty
}

penlik1 <- function(resimi, pen1, Xmi, thetai, lambda1, sigma1, n)
{
  resi <- resimi - Xmi %*% thetai
  0.5 * as.numeric(crossprod(resi)) / n + pen1 +
    lambda1 * exp(-max(abs(thetai)) / sigma1) * sqrt(sum(thetai ^ 2))
}

penlik2 <- function(resiint, Xi, theta.list, eta.list, groupsizes, lambda, sigma, n, gamma)
{
  if (is.null(gamma))
    gamma <- eta.to.gamma(theta.list, eta, groupsizes)$gamma
  resi <- resiint - Xi %*% gamma
  lik <- as.numeric(crossprod(resi) * 0.5) / n
  pen2 <- lambda * pen(eta.list, sigma)
  lik + pen2
}

## compute the penalty term
pen <- function(coef.list, sigma)
{
  sum <- 0
  for (coef in coef.list)
    sum <- sum + exp(-max(abs(coef)) / sigma) * sqrt(sum(coef ^ 2))
  sum
}

## update coefficients
update.alpha <- function(Y, Xm, Xi, Z, ZTZinv, theta, gamma)
{
  ZTZinv %*% t(Z) %*% (Y - Xm %*% theta - Xi %*% gamma)
}

update.theta <- function(resi, Xm_list, Xi_list, theta_list, eta_list, S, index_list, groupsize, sigma1, lambda1, halfmax, n, mainzero_HiGLASSO)
{
  for (i in 1:S) {
    if (sum(abs(theta_list[[i]])) == 0)
	  theta_list[[i]] <- rep(mainzero_HiGLASSO, groupsize[i])
  }
  pen1 <- lambda1 * pen(theta_list, sigma1)
  oldpenlik1 <- as.numeric(crossprod(resi)) / 2 / n + pen1
  dlist <- constructdlist(coef_list = theta_list, sigma = sigma1)
  for (i in 1:S) {
    thetai <- theta_list[[i]]
    Xtmp <- Xm_list[[i]]
    ind_tmp <- index_list[[i]]
    indcount <- 1
    if (i > 1) {
      for (j in 1:(i - 1)) {
        ind <- ind_tmp[indcount]
        Xtmp <- Xtmp + Xi_list[[ind]] %*% diag(c(eta_list[[ind]])) %*% ((theta_list[[j]]) %x% diag(groupsize[i]))
        indcount <- indcount + 1
      }
    }
    if (i < S) {
      for (j in (i + 1):S) {
        ind <- ind_tmp[indcount]
        Xtmp <- Xtmp + Xi_list[[ind]] %*% diag(c(eta_list[[ind]])) %*% (diag(groupsize[i]) %x% (theta_list[[j]]))
        indcount <- indcount + 1
      }
    }
    resi <- resi + Xtmp %*% theta_list[[i]]
    pen1 <- pen1 - lambda1 * exp(-max(abs(thetai)) / sigma1) * sqrt(sum(thetai^2))
    d_m <- dlist[[i]]
    D_m <- diag(c(abs(d_m)))
    C_m <- abs(d_m) * (1 - sign(d_m)) * theta_list[[i]]
    candthetai <- ginv(crossprod(Xtmp) + lambda1*D_m) %*% (crossprod(Xtmp, resi) + lambda1 * C_m)
    newpenlik1 <- Inf
    halfcount <- 0
    while (oldpenlik1 < newpenlik1 && halfcount < halfmax) {
      if (newpenlik1 < Inf)
        candthetai <- (thetai + candthetai) / 2
      newpenlik1 <-  penlik1(resi, pen1, Xtmp, candthetai, lambda1, sigma1, n)
      halfcount <- halfcount + 1
    }
	newpenlik0 <-  penlik1(resi, pen1, Xtmp, rep(0, groupsize[i]), lambda1, sigma1, n)
	if (newpenlik0 <= newpenlik1)
	  candthetai <- rep(0, groupsize[i])
	theta_list[[i]] <- candthetai
    resi <- resi - Xtmp %*% candthetai
    pen1 <- pen1 + lambda1 * exp(-max(abs(candthetai)) / sigma1) * sqrt(sum(candthetai ^ 2))
    dlist[[i]] <- constructd(coefvec = candthetai, sigma = sigma1)
    oldpenlik1 <- newpenlik1
  }
  return(theta_list)
}

update.eta <- function(resiint, Xi, theta_list, eta, eta_list, S, lambda2, sigma2, groupsize, halfmax, n, intzero_HiGLASSO)
{
    oldpenlik2 <- penlik2(resiint, Xi, theta_list, eta, eta_list, groupsize, lambda2, sigma2, n)
    thetaprod <- c()
    ind <- 1
    for(i in 1:(S - 1)) {
        for(j in (i + 1):S) {
            thetaprod <- c(thetaprod, (theta_list[[i]])%x%(theta_list[[j]]))
		if(sum(abs(eta_list[[ind]])) == 0) {
			eta_list[[ind]] <- rep(intzero_HiGLASSO, groupsize[i]*groupsize[j])
		}
		ind <- ind + 1
        }
    }
    Xtmp <- Xi%*%diag(thetaprod)
    dlist <- constructdlist(coef_list = eta_list, sigma = sigma2)
    d <- unlist(dlist)
    D <- diag(c(abs(d)))
    C <- abs(d)*(1 - sign(d))*unlist(eta_list)
    try(candeta <- solve(crossprod(Xtmp) + lambda2*D)%*%(crossprod(Xtmp, resiint) + lambda2*C), silent = TRUE)
    if(!exists('candeta')) {
        candeta <- eta
    }
    newpenlik2 <- Inf
    halfcount <- 0
    while(oldpenlik2 < newpenlik2 && halfcount < halfmax) {
        if(newpenlik2 < Inf) {
            candeta <- (eta + candeta) / 2
        }
        candeta_list <- etatolist(eta = candeta, groupsize = groupsize)
        newpenlik2 <-  penlik2(resiint = resiint, Xi = Xi, theta_list = theta_list, eta = candeta, eta_list = candeta_list, groupsize = groupsize, lambda2 = lambda2, sigma2 = sigma2, n = n)
        halfcount <- halfcount + 1
    }
    ind <- 1
    gamma <- etatogamma(theta_list = theta_list, eta = candeta, groupsize = groupsize)$gamma
    start <- 1
    for(i in 1:(S - 1)) {
        for(j in (i + 1):S) {
		gamma2 <- gamma
		gamma2[start:(start + groupsize[i]*groupsize[j] - 1)] <- rep(0, groupsize[i]*groupsize[j])
		candeta2_list <- candeta_list
		candeta2_list[[ind]] <- rep(0, groupsize[i]*groupsize[j])
		# candeta2 <- unlist(candeta2_list)
		zeropenlik2 <- penlik2(resiint = resiint, Xi = Xi, theta_list = theta_list, eta_list = candeta2_list,
			groupsize = groupsize, lambda2 = lambda2, sigma2 = sigma2, n = n, gamma = gamma2)
		if(zeropenlik2 <= newpenlik2) {
			newpenlik2 <- zeropenlik2
			# candeta <- candeta2
			candeta_list <- candeta2_list
			gamma <- gamma2
		}
		ind <- ind + 1
		start <- start + groupsize[i]*groupsize[j]
        }
    }

    # eta <- candeta
    eta_list <- candeta_list
    eta <- unlist(eta_list)
    return(list(eta = eta, eta_list = eta_list))
}

## obtain d
constructdlist <- function(coef.list, sigma)
{
  ngroup <- length(coef.list)
  d.list <- lapply(coef.list, function(x) rep(exp(-max(abs(x)) / sigma) / sqrt(sum(x^2)), length(x)))
  for (i in 1:ngroup) {
    coef_tmp <- coef.list[[i]]
    ind <- which.max(abs(coef_tmp))
    d_list[[i]][ind] <- d_list[[i]][ind] - exp(-max(abs(coef_tmp)) / sigma)*sqrt(sum(coef_tmp^2)) / abs(coef_tmp[ind]) / sigma
  }
  d.list
}

constructd <- function(coef.vec, sigma)
{
  d <- exp(-max(abs(coef.vec)) / sigma) / sqrt(sum(coef.vec^2))
  ind <- which.max(abs(coef.vec))
  d[ind] <- d[ind] - exp(-max(abs(coef.vec)) / sigma) * sqrt(sum(coef.vec^2)) / abs(coef.vec[ind]) / sigma
  d
}


################################################################################################
## Why is there Xm and Xm_list? Just as an option for user to enter one or the other?
## groupsize is not defined in arguments for HiGLASSO, is that a problem?
## Where is elastic net intialization?
## Adaptive Elastic Net for initialization of beta and eta? Just throw in non-linear expansions
## for main effects and interactions? I don't see where Aaron did this.
## glinternet output doesn't make any sense to me.
################################################################################################


## main function
IntHierGrp <- function(Y, Xm = NULL, Xm_list = NULL, Xi_list = NULL, Z,
	                     inittheta_list, initeta_list, initalpha, lambda1 = 1,
					     lambda2 = 1, sigma1 = 1, sigma2 = 1, delta = 10^(-5),
					     countmax = 1000, halfmax = 100, mainzero_HiGLASSO,
					     intzero_HiGLASSO)
{
    ## sanity check
    if (is.null(Xm) && is.null(Xm_list)) {
        stop("No design matrix")
    }
	## setup
	S <- length(groupsize)
    theta_list <- inittheta_list
    eta_list <- initeta_list
    alpha <- initalpha

	## obtain Xm_list and Xm
    if (is.null(Xm_list)) {
        Xm_list <- vector("list", S)
        start <- 1
        for (i in 1:S) {
            Xm_list[[i]] <- Xm[, start:(start + groupsize[i] - 1)]
            start <- start + groupsize[i]
        }
    } else {
        Xm <- do.call("cbind", Xm_list)
    }
	n <- dim(Xm)[1]

	## obtain Xi and create index list and gamma list
	gamma_list <- vector("list", S*(S - 1) / 2)
    ind_list <- vector("list", S)
	ind <- 1
   	start <- 1
	for(i in 1:(S - 1)) {
		for(j in (i + 1):S) {
            tmpsize <- groupsize[i]*groupsize[j]
            gamma_list[[ind]] <- eta_list[[ind]]*(theta_list[[i]] %x% theta_list[[j]])
            start <- start + tmpsize
            ind_list[[i]] <- c(ind_list[[i]], ind)
            ind_list[[j]] <- c(ind_list[[j]], ind)
			ind <- ind + 1
		}
	}

	if(is.null(Xi_list)) {
		Xi_list <- vector("list", S*(S - 1) / 2)
		ind <- 1
		for(i in 1:(S - 1)) {
			for(j in (i + 1):S) {
				Xi_list[[ind]] <- t(sapply(1:n, function(x) (Xm_list[[i]][x, ]) %x% (Xm_list[[j]][x, ])))
				ind <- ind + 1
			}
		}
	}
	Xi <- do.call("cbind", Xi_list)

    ## unlist theta list, eta list, and gamma list
    theta <- unlist(theta_list)
    eta <- unlist(eta_list)
    gamma <- unlist(gamma_list)

    ## obtain Z related quantities
    ZTZ <- crossprod(Z)
    ZTZinv <- solve(ZTZ)

    ## iterative
    penlikdiff <- Inf
    newpenlik <- Inf
    count <- 0
    while (abs(penlikdiff) > delta && count < countmax) {
	  t <- Sys.time()
      alpha <- update.alpha(Y, Xm, Xi, Z, ZTZinv, theta, gamma)
      resi <- Y - Z %*% alpha - Xm %*% theta - Xi %*% gamma
      ## obtain residuals
      theta_list <- update.theta(resi, Xm_list, Xi_list, theta_list, eta_list,
			                       S, ind_list, groupsize, sigma1, lambda1,
								   halfmax, n, mainzero_HiGLASSO)
 	  theta <- unlist(theta_list)
      resiint <- Y - Z %*% alpha - Xm %*% theta
	  update.eta <- function(resiint, Xi, theta_list, eta, eta_list, S, lambda2, sigma2, groupsize, halfmax, n, intzero_HiGLASSO)
      eta_obj <- update.eta(resiint, Xi, theta_list, eta, eta_list, S, lambda2, sigma2, groupsize, halfmax, n, intzero_HiGLASSO)
      eta <- eta_obj$eta
      eta_list <- eta_obj$eta_list
      gamma_obj <- eta.to.gamma(theta_list, eta, groupsize)
      gamma <- gamma_obj$gamma
      ## update penalized likelihood
      oldpenlik <- newpenlik

      newpenlik <- penlik(Y, Xm, Xi, Z, theta, eta, alpha, theta_list, eta_list,
		                    gamma, groupsize, lambda1, lambda2, sigma1, sigma2, n)
      penlikdiff <- oldpenlik - newpenlik
      count <- count + 1
	  print(c(count, penlikdiff, Sys.time() - t))
    }
    gamma_list = gamma_obj$gamma_list
    resi <- Y - Z %*% alpha - Xm %*% theta - Xi %*% gamma
    mspe <- as.numeric(crossprod(resi)) / 2 / n
    return(list(alpha = alpha, theta = theta, theta_list = theta_list, gamma = gamma, gamma_list = gamma_list, eta = eta, eta_list = eta_list, mspe = mspe))
}
