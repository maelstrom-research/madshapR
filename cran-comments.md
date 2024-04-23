## R CMD check results

0 errors | 0 warnings | 0 note

> NOTE : there was an error in the previous attempt for this version. The 
> previous attempt has been cancelled, and I propose a corrected version.
> 
> In this corrected version, the auto-check problems in mlstrOpalr and Rmonize 
> detected when checking the first order strong reverse dependencies is this 
> expected. 
> All maintainers of affected packages have been informed in advance. The 
> problems detected only affect examples used in the packages, and specific
> use cases such as empty input parameters. The packages Rmonize and mlstrOpalr
> are being updated accordingly right after madshapR.
> Thanks.

# Latest submission : madshapR 1.1.0

## Bug fixes and improvements

- Some correction after git issues comments.

- To avoid confusion with help(function), the function `madshapR_help()` has been 
renamed `madshapR_website()`.

- set a minimum dplyr dependence to avoid bugs
