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
