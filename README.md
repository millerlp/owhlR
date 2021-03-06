
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Open Wave Height Logger data processing with R

The package `owhlR` contains functions to post-process raw csv data
files produced by an Open Wave Height Logger (OWHL) pressure sensor data
logger. See the vignette `OWHL-workflow` for a walk-through of the
steps.

This package depends on the `oceanwaves` package
<https://github.com/millerlp/oceanwaves> and its downstream dependencies
for the calculation of wave statistics after the raw pressure data have
been imported and quality-checked.

To install the development version of this package from within R, first
install the package `devtools`
<https://CRAN.R-project.org/package=devtools> and then install this
package from Github:

``` r
install.packages('devtools')
library(devtools)
install_github('millerlp/owhlR', build_vignettes = TRUE)
```

To open the `OWHL-workflow` vignette, run the following commands

``` r
library(owhlR)
vignette('OWHL-workflow', package = 'owhlR')
```

For more information on the Open Wave Height Logger project, see
<https://lukemiller.org/?s=owhl> and the Github repository:
<https://github.com/millerlp/OWHL>
