
<!-- README.md is generated from README.Rmd. Please edit that file -->

# madshapR

<!-- badges: start -->

[![R-CMD-check](https://github.com/maelstrom-research/madshapR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maelstrom-research/madshapR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of madshapR is to provide functions to support rigorous
processes in data cleaning, evaluation, and documentation across
datasets from different studies based on Maelstrom Research guidelines.
The package includes the core functions to evaluate and format the main
inputs that define the process, diagnose errors, and summarize and
evaluate datasets and their associated data dictionaries. The main
outputs are clean datasets and associated metadata, and tabular and
visual summary reports.

# Get started

## Install the package

To download the package, you need to have an account on
<a href="https://github.com/" target="_blank">GitHub</a> and create a
<a href="https://github.com/settings/tokens" target="_blank">personal
access token</a> (in the format “ghp_xxx”). To use the package, you will
need to be added to the list of authorized users. To request access,
contact us and send us your Github username.

You can send us your Github name using the
<a href="https://www.maelstrom-research.org/contact" target="_blank">following
link</a> so we can add you to our authorized list of users.

``` r
# To update the R package in your R environment you may first need to remove it, 
# and use the exit command quit() to terminate the current R session.

# To install the R package:
devtools::install_github(
  repo = "https://github.com/maelstrom-research/madshapR@testing",
  auth_token = "ghp_YouNeedaValidGithubTOKEN!!!",
  force = TRUE)
```

``` r
library(madshapR) 

#if you need help with the package, please use:
madshapR_help()
```
