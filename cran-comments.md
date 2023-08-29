## R CMD check results

0 errors | 0 warnings | 0 note


# Latest submission : madshapR 1.0.2

## Creation of NEWS feed !!

Addition of `NEWS.md` for the development version use "(development version)".

## Bug fixes and improvements

* Some improvements in the documentation of the package has been made.
* internal call of libraries (using '::') has been replaced by proper import in
the declaration function.

* get functions in [fabR](https://guifabre.github.io/fabR-documentation/) have
been changed in its last release. the functions using them as dependancies (
'check_xxx()') have been updated accordingly.

* DEMO files no longer include harmonization files that are now in the package
[harmonizR](https://maelstrom-research.github.io/harmonizR-documentation/)


## Dependency changes

**New Imports:** lifecycle

**No longer in Imports:** xfun

## New functions


These functions are imported from [fabR](https://guifabre.github.io/fabR-documentation/)

* `bookdown_template()` replaces the deprecated function `bookdown_template()`.

* `bookdown_render()` which renders a Rmd collection of files into a docs/index.html
website.

* `bookdown_open()` Which allows to open a docs/index.html document when the bookdown
is rendered

This separation into 3 functions will allow future developments, such as render as
a ppt or pdf.

## deprecated functions

Due to another package development 
(see [fabR](https://guifabre.github.io/fabR-documentation/)),
The function `open_visual_report()` has been deprecated in favor of 
`bookdown_open()` imported from fabR package.

--------------------------------------------------------------------------------

## Previous submission : madshapR 1.0.0


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Correction made in the second attempt of release (url was not good)
* lightned the examples so they can run under 5 sec


>If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>

Done

>Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation.
-> madshapR_help.Rd: \value
-> open_visual_report.Rd: \value

Done 

>\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.
Some code lines in examples are commented out. Please never do that.
-> open_visual_report.Rd

Done
