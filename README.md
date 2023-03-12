
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href="http://www.thecodingdocs.com" width="25%"><img
src="man/figures/TCD.png" alt="TheCodingDocs.com" /></a>

# rosyredcap <img src="man/figures/logo.png" align="right" height="200" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of rosyredcap is to select ICD10 codes! We are still in
development.

## Installation

You can install the development version of rosyredcap like so:

``` r
# install remotes package if you don't have it
# install.packages("remotes") 
remotes::install_github("brandonerose/rosyredcap")
```

If you have any issues above download the most recent version of R at
RStudtio and update all packages in RStudio. See
[thecodingdocs.com/r/getting-started](https://www.thecodingdocs.com/r/getting-started "R Getting Started").

## Run

This is how you get REDCap turned into an R database - only for basic
projects at this time

``` r
library("rosyredcap")

redcap_link<-"https://redcap.miami.edu/" #or change to your institution

set_dir(getwd()) #this is where files where drop, default is project but pick another path if needed

DB<-update_DB(token = "yourNEVERshareTOKENfromREDCapAPI") 
#DB<-update_DB(token = "yourNEVERshareTOKENfromREDCapAPI",force=T) #force for autmatic update

DB %>% drop_redcap_dir() #drops excel files with links to dir
```

## Links

The rosyredcap package is at
[github.com/brandonerose/rosyredcap](https://github.com/brandonerose/rosyredcap "rosyredcap R package")
See instructions above. Install remotes and install rosyredcap

Donate if I helped you out and want more development (anything helps)!
[account.venmo.com/u/brandonerose](https://account.venmo.com/u/brandonerose "Venmo Donation")

For more R coding visit
[thecodingdocs.com/](https://www.thecodingdocs.com/ "TheCodingDocs.com")

Follow us on Twitter
[twitter.com/TheCodingDocs](https://twitter.com/TheCodingDocs "TheCodingDocs Twitter")

Follow me on Twitter
[twitter.com/BRoseMDMPH](https://twitter.com/BRoseMDMPH "BRoseMDMPH Twitter")
