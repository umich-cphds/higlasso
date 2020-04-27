This is a re-submission.

## Test environments
* Ubuntu 16.04 (travis): r-devel (2020-04-27 r78313), r-release (4.0.0), r-oldrelease (3.6.2)

* winbuilder: r-devel (2020-04-27 r78313), r-release (4.0.0)

## R CMD check results
> There were no ERRORs or WARNINGs.

> There was 1 NOTE:

> Maintainer: ‘Alexander Rix <alexrix@umich.edu>’

> New submission

## Reviewer comments

> Please always explain all acronyms/abbreviations in the description text
> in the Description field of the DESCRIPTION file.

LASSO has been replaced by Least Absolute Shrinkage and Selection Operator.

> Please add \value to all .Rd files for exported functions and explain
> the functions results in the documentation.
> f.i.: print.cv.higlasso.Rd
> If a function does not return a value, please document that too, e.g.
> \value{None}.

print.cv.higlasso now has a value

> ** running tests for arch 'i386' ... [532s] OK
> ** running tests for arch 'x64' ... [482s] OK
> ** checking re-building of vignette outputs ... [278s] OK

> This is much more than the CRAN threshold oif 10 min for a package
> check, so please reduce by using toy data/few iterations and perhaps run
> less important tests only locally.

This has been reduced (according to winbuilder) to
** running tests for arch 'i386' ... [41s] OK
** running tests for arch 'x64' ... [44s] OK
** checking re-building of vignette outputs ... [251s] OK
