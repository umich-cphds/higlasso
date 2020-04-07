This is a re-submission.

## Test environments
* Ubuntu 16.04 (travis): r-devel (2020-03-13 r77948), r-release (3.6.2), r-oldrelease (3.5.3)

* winbuilder: r-devel (2020-03-26 r78078), r-release (3.6.3)

## R CMD check results
> There were no ERRORs or WARNINGs.

> There was 1 NOTE:

> Maintainer: ‘Alexander Rix <alexrix@umich.edu>’

> New submission

## Reviewer comments

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
