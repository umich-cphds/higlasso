<!-- Badges -->
[![CRAN
Version](https://img.shields.io/cran/v/mianet?style=flat-square&color=blue&label=CRAN)](https://cran.r-project.org/package=higlasso)
[![GitHub
Release](https://img.shields.io/github/v/release/umich-cphds/higlasso?include_prereleases&label=Github&style=flat-square&color=blue)](https://github.com/umich-cphds/higlasso)
[![Travis
CI](https://img.shields.io/travis/umich-cphds/higlasso?style=flat-square)](https://travis-ci.org/umich-cphds/higlasso)

Hierarchical Integrative Group LASSO
====================================

Environmental health studies are increasingly measuring multiple
pollutants to characterize the joint health effects attributable to
exposure mixtures. However, the underlying dose-response relationship
between toxicants and health outcomes of interest may be highly
nonlinear, with possible nonlinear interaction effects. Hierarchical
integrative group LASSO (HiGLASSO) is a general shrinkage and selection
framework to identify noteworthy nonlinear main and interaction effects
in the presence of group structures among a set of exposures.

Installation
------------

`higlasso` can be installed via Github using `devtools`

    # install.packages("devtools")
    devtools::install_github("umich-cphds/higlasso")

You'll need a working C++11 compiler, which can obtained by installing
Xcode on MacOS, and RTools on Windows.

Example
-------

`higlasso` can be slow, so it may may be beneficial to tweak some its
settings (for example, `nlambda1` and `nlambda2`) to get a handle on how
long the method will take before running the full model.

    library(higlasso)

    set.seed(48109)

    X <- as.matrix(higlasso.df[, paste0("V", 1:10)])
    Y <- higlasso.df$Y
    Z <- matrix(1, nrow(X))

    # This can take a bit of time
    fit <- cv.higlasso(Y, X, Z)

    print(fit)

    ## 'cv.higlaso' fit:
    ## Average cross validation error for each (lambda1, lambda2)
    ##           l2.1     l2.2     l2.3     l2.4     l2.5     l2.6     l2.7     l2.8
    ## l1.1  28.80316 28.52650 25.70143 24.39285 24.09593 20.15801 18.75599 18.19980
    ## l1.2  28.74086 28.30145 25.58400 23.45703 20.03493 18.48962 19.16629 17.92222
    ## l1.3  26.51969 26.72091 23.12893 19.84514 18.06675 18.65337 18.32306 17.52575
    ## l1.4  25.51772 25.77686 23.45064 19.98032 18.67845 17.87906 17.79486 18.85360
    ## l1.5  25.90018 25.86865 23.82170 20.54826 19.14685 18.24189 17.82151 15.48574
    ## l1.6  26.02175 25.87087 23.98798 21.05609 19.44195 18.42754 17.45593 17.97917
    ## l1.7  25.95308 25.80337 24.39706 21.15023 20.50979 18.17171 16.21298 17.01503
    ## l1.8  25.83173 25.92854 24.95724 20.60133 19.65546 18.02608 16.05186 15.00402
    ## l1.9  25.78995 25.87314 24.91311 20.69738 19.25751 18.26398 15.51476 15.05535
    ## l1.10 25.83080 25.92750 24.97252 20.41357 20.16386 17.95384 16.96716 15.55314
    ##           l2.9    l2.10
    ## l1.1  17.44473 18.35613
    ## l1.2  17.32831 17.44720
    ## l1.3  17.52913 19.18150
    ## l1.4  18.16910 13.97345
    ## l1.5  14.86815 15.13851
    ## l1.6  14.25933 14.78646
    ## l1.7  14.80839 14.19361
    ## l1.8  14.37435 14.37588
    ## l1.9  14.75224 13.90420
    ## l1.10 14.44388 14.73295
    ## Lambda min:
    ## 0.210345 0.1707173 
    ## Lambda 1 SE:
    ## 0.4093077 0.1707173

Bugs
----

If you encounter a bug, please open an issue on the
[Issues](https://github.com/umich-cphds/higlasso/issues) tab on Github
or send us an email.

Contact
-------

For questions or feedback, please email Jonathan Boss at
<bossjona@umich.edu> or Alexander Rix <alexrix@umich.edu>.

References
----------
