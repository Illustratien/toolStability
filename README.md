
<!-- README.md is generated from README.Rmd. Please edit that file -->

## `toolStability`: Tool for Stability Indices Calculation <img src="https://raw.githubusercontent.com/Illustratien/toolStability/master/inst/extdata/toolStability.png" align="right" alt="logo" width="200" height = "200" style = "padding: 10px; border: none; float: right;">

###### Version : [0.1.1](https://illustratien.github.io/toolStability/articles/toolStability.html#version-history); License: [GPL-3](https://www.r-project.org/Licenses/)

##### *Wang, T-C. and Chen, T-W.*

<!-- badges: start -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5.0-6666ff.svg?logo=R)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/toolStability)](https://cran.r-project.org/package=toolStability)

[![develVersion](https://img.shields.io/badge/devel%20version-0.1.1-orange.svg)](https://github.com/Illustratien/toolStability)
[![Github Code
Size](https://img.shields.io/github/languages/code-size/Illustratien/toolStability.svg)](https://github.com/Illustratien/toolStability)
[![R-CMD-check](https://github.com/Illustratien/toolStability/workflows/R-CMD-check/badge.svg)](https://github.com/Illustratien/toolStability/actions)
[![Last-changedate](https://img.shields.io/badge/last%20change-2022--11--02-yellowgreen.svg)](https://github.com/Illustratien/toolStability/commits/master)
[![DOI](https://zenodo.org/badge/203346020.svg)](https://zenodo.org/badge/latestdoi/203346020)
[![Website -
pkgdown](https://img.shields.io/badge/website-githubpage-green)](https://Illustratien.github.io/toolStability/)
[![.](https://raw.githubusercontent.com/vitr/google-analytics-beacon/master/static/badge-flat.gif)](https://github.com/Illustratien/google-analytics-beacon)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/toolStability?color=green)](https://CRAN.R-project.org/package=toolStability)
[![Codecov test
coverage](https://codecov.io/gh/Illustratien/toolStability/branch/master/graph/badge.svg)](https://codecov.io/gh/Illustratien/toolStability?branch=master)
<!-- badges: end -->

## Description

Tools to calculate stability indices with parametric, non-parametric and
probabilistic approaches. The basic data format requirement for
‘toolStability’ is a data frame with 3 columns including numeric trait
values, genotype,and environmental labels. Output format of each
function is the dataframe with chosen stability index for each genotype.
Function “table_stability” offers the summary table of all stability
indices in this package. Sample dataset in this package is from:
Casadebaig P, Zheng B, Chapman S et al. (2016) \<doi:
10.1371/journal.pone.0146385\>. Indices used in this package are from:
Döring TF, Reckling M (2018) \<doi: 10.1016/j.eja.2018.06.007\>.
Eberhart SA, Russell WA (1966) \<doi:
10.2135/cropsci1966.0011183X000600010011x\>. Eskridge KM (1990) \<doi:
10.2135/cropsci1990.0011183X003000020025x\>. Finlay KW, Wilkinson GN
(1963) \<doi: 10.1071/AR9630742\>. Hanson WD (1970) Genotypic stability.
\<doi: 10.1007/BF00285245\>. Lin CS, Binns MR (1988)
<https://cdnsciencepub.com/doi/abs/10.4141/cjps88-018>. Nassar R, Hühn M
(1987). Pinthus MJ (1973) \<doi: 10.1007/BF00021563\>. Römer T (1917).
Shukla GK (1972). Wricke G (1962).

## Installation

The package can be installed from CRAN as follows:

``` r
install.packages('toolStability', dependencies=TRUE)
```

The development version can be installed from github as follows:

``` r
if (!require('devtools')) install.packages('devtools')
devtools::install_github("Illustratien/toolStability")
# or
if (!require('remotes')) install.packages('remotes')
remotes::install_github("Illustratien/toolStability")
```

## Detailed tutorial

For a detailed tutorial (vignette) on how to used this package type:

``` r
browseVignettes(package = 'toolStability')
```

The vignette for the latest version is also available
[online](https://illustratien.github.io/toolStability/articles/toolStability.html).

## What’s new

To know whats new in this version type:

``` r
news(package='toolStability')
```

## Links

[CRAN page](https://cran.r-project.org/package=toolStability)

[Documentation website](https://illustratien.github.io/toolStability/)

[Zenodo DOI](https://zenodo.org/badge/latestdoi/203346020)

## Citing `toolStability`

To cite the methods in the package use:

``` r
citation("toolStability")
```


    To cite the R package 'toolStability' in publications use:

      Wang, T-C. and Chen, T-W. ( 2022 ).  toolStability: Tool for
      stability indices calculation.  R package version 0.1.1 ,
      https://github.com/Illustratien/toolStability/
      https://cran.r-project.org/package=toolStability .

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {toolStability: Tool for Stability Indices Calculation},
        author = {T.-C. Wang and T.-W. Chen},
        year = {2022},
        note = {R package version 0.1.1},
        note = {https://github.com/Illustratien/toolStability/},
        note = {https://cran.r-project.org/package=toolStability},
      }

    This free and open-source software implements academic research by the
    authors and co-workers. If you use it, please support the project by
    citing the package.
