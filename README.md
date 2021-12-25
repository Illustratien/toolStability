
## `toolStability`: Tool for Stability Indices Calculation <img src="https://raw.githubusercontent.com/Illustratien/toolStability/master/inst/extdata/toolStability.png" align="right" alt="logo" width="200" height = "200" style = "padding: 10px; border: none; float: right;">

    Warning: package 'testthat' was built under R version 4.0.4

###### Version : \[0.1.0\]; License: [GPL-3](https://www.r-project.org/Licenses/)

##### *Wang, T-C. and Chen, T-W.*

------------------------------------------------------------------------

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/toolStability)](https://cran.r-project.org/package=toolStability)
[![Dependencies](https://tinyverse.netlify.com/badge/toolStability)](https://cran.r-project.org/package=toolStability)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/toolStability?color=green)](https://CRAN.R-project.org/package=toolStability)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.0-orange.svg)](https://github.com/Illustratien/toolStability)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/Illustratien/toolStability.svg)](https://github.com/Illustratien/toolStability)
[![R-CMD-check](https://github.com/Illustratien/toolStability/workflows/R-CMD-check/badge.svg)](https://github.com/Illustratien/toolStability/actions)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--12--25-yellowgreen.svg)](https://github.com/Illustratien/toolStability/commits/master)
[![DOI](https://zenodo.org/badge/203346020.svg)](https://zenodo.org/badge/latestdoi/203346020)
<!-- [![Website - pkgdown](https://img.shields.io/website-up-down-green-red/https/Illustratien.github.io/toolStability.svg)](https://Illustratien.github.io/toolStability/) -->
[![.](https://pro-pulsar-193905.appspot.com/UA-116205606-1/welcome-page)](https://github.com/Illustratien/google-analytics-beacon)
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/Illustratien/toolStability) -->
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/Illustratien/toolStability/total.svg)] -->
<!-- [![Rdoc](https://www.rdocumentation.org/badges/version/toolStability)](https://www.rdocumentation.org/packages/toolStability) -->

------------------------------------------------------------------------

## Description

Tools to calculate stability indices with parametric, non-parametric and
probabilistic approaches. The basic data format requirement for
‘toolStability’ is a data frame with 3 columns including numeric trait
values, genotype,and environmental labels. Output format of each
function is the dataframe with chosen stability index for each genotype.
Function “table_stability” offers the summary table of all stability
indices in this package. Sample dataset in this package is from:
Casadebaig P, Zheng B, Chapman S et al. (2016) \<doi:
10.1371/journal.pone.0146385>. Indices used in this package are from:
Doering TF, Reckling M (2018) \<doi: 10.1016/j.eja.2018.06.007>.
Eberhart SA, Russell WA (1966) \<doi:
10.2135/cropsci1966.0011183X000600010011x>. Eskridge KM (1990) \<doi:
10.2135/cropsci1990.0011183X003000020025x>. Finlay KW, Wilkinson GN
(1963) \<doi: 10.1071/AR9630742>. Hanson WD (1970) Genotypic stability.
\<doi: 10.1007/BF00285245>. Lin CS, Binns MR (1988)
<https://cdnsciencepub.com/doi/abs/10.4141/cjps88-018>. Nassar R, Huehn
M (1987). Pinthus MJ (1973) \<doi: 10.1007/BF00021563>. Roemer T (1917).
Shukla GK (1972). Wricke G (1962).

## Installation

The package can be installed from CRAN as follows:

``` r
if (!require('devtools')) install.packages('devtools')
install.packages('toolStability', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
devtools::install_github("Illustratien/toolStability")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = 'toolStability')
```

<!-- The vignette for the latest version is also available [online](https://Illustratien.github.io/toolStability/articles/Introduction.html). -->

## What’s new

To know whats new in this version type:

``` r
news(package='toolStability')
```

## Links

[CRAN page](https://cran.r-project.org/package=toolStability)

[Github page](https://github.com/Illustratien/toolStability)

<!-- [Documentation website](https://Illustratien.github.io/toolStability/) -->

[Zenodo DOI](https://zenodo.org/badge/latestdoi/203346020)

## CRAN checks

<!-- ```{r, echo = FALSE, results='asis'} -->
<!-- library(kableExtra, warn.conflicts = FALSE) -->
<!-- kosdf <- kbl(osdf[, c("Flavour", "CRAN check")], row.names = FALSE) -->
<!-- kosdf2 <- kable_styling(kosdf, bootstrap_options = c("striped", "hover"), -->
<!--               full_width = F, position = "left") -->
<!-- # pack_rows(kosdf2, index = c("[![Linux](https://shields.io/badge/Linux--9cf?logo=Linux&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)" = 6, -->
<!-- #                             "[![Solaris](https://shields.io/badge/Solaris--9cf?logo=Oracle&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)" = 1, -->
<!-- #                             "[![Windows](https://shields.io/badge/Windows--9cf?logo=Windows&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)" = 3, -->
<!-- #                             "[![MacOS](https://shields.io/badge/MacOS--9cf?logo=Apple&style=social)](https://cran.r-project.org/web/checks/check_results_ammistability.html)" = 2)) -->
<!-- ``` -->

## Citing `toolStability`

To cite the methods in the package use:

``` r
citation("toolStability")
```


    To cite the R package 'toolStability' in publications use:

      Wang, T-C. and Chen, T-W. ( 2021 ).  toolStability: Tool for
      stability indices calculation.  R package version 0.1.0 ,
      https://github.com/Illustratien/toolStability/
      https://cran.r-project.org/package=toolStability .

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {toolStability: Tool for Stability Indices Calculation},
        author = {T.-C. Wang and T.-W. Chen},
        year = {2021},
        note = {R package version 0.1.0},
        note = {https://github.com/Illustratien/toolStability/},
        note = {https://cran.r-project.org/package=toolStability},
      }

    This free and open-source software implements academic research by the
    authors and co-workers. If you use it, please support the project by
    citing the package.
