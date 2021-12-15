
# toolStability
This is an r package for calculating parametric, non-parametric, and probabilistic stability indices.

##package structure overview

toolStability contains different functions to calculate stability indices, including:

 1.  adjusted coefficient of variation
 2.  coefficient of determination
 3.  coefficient of regression
 4.  deviation mean squares
 5.  ecovalence
 6.  environmental variance
 7.  genotypic stability
 8.  genotypic superiority measure
 9.  safety first index
 10. stability variance
 11. variance of rank


##function input tutorial

1. Data preparation 

In order to calculate stability index, you will need to prepare a data frame with 3 columns containing trait, genotype, and environment.


  * trait:       numeric and continuous, trait value to be analyzed.
  * genotype:    character or factor, labeling different genotypic varieties.
  * environment: character or factor, labeling different environments.

2. Input formats of function

Most of the functions in the package work with the following format: 

function (data = Data,
         trait = "Trait_Column_Name",
         genotype = "Genotype_Column_Name",
         environment = "Environment_Column_Name")

For calculation of probabilistic stability index "safety_first_index", an additional parameter "lambda" is required.

  * lambda:      minimal acceptable value of trait that the user expected from crop across environments. Lambda should between the ranges of trait value. Under the assumption of trait is normally distributed, safety first index is calculated based on the probability of trait below lambda across the environments for each genotype.

3. Function Features

  Function “table_stability" generates the summary table containing all the stability indices in the package for every genotype, also including the mean yield and normality check results for the trait of each genotype across all the environments. 

  User can specify the interested combination of environments by entering a vector with column names of environmental factors. See examples for more details.

4. Equations for stability indices

  The equation of each stability index can be found in vignette of the toolStability package.
  
  vignette("Vignette", package = "toolStability")

## dataset description
The default data set 'Data' is the subset of APSIM simulated wheat dataset, which includes 5 genotypes in 4 locations for 4 years, with 2 nitrogen application rates, 2 sowing dates, and 2 CO2 levels of treatments (Casadebaig et al., 2016).

'Data' in this package is A data frame with 640 observations and 8 variables.

Trait            Wheat yield (kg.ha-1). 
Genotype     5   varieties.
Environment  128 unique combination of environments for each genotype.
Year         4   years.
Sites        4   locations.
Nitrogen     2   nitrogen application levels.
CO2          2   CO2 concentration levels.
Sowing       2   sowing dates.

## Examples 

```R
---
author: marse
date: 2021-12-10
output: "reprex::reprex_document"
title: grave-cobra_reprex.R
---

``` r
rm(list=ls())
library(toolStability)
### load data
data("Data")
### check the structure of sample dataset
### be sure that the trait is numeric!!!
dplyr::glimpse(Data)
#> Rows: 640
#> Columns: 8
#> $ Genotype    <fct> 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864~
#> $ Yield       <dbl> 1278.6, 1746.0, 1753.9, 1851.8, 2176.6, 2783.3, 3113.3, 27~
#> $ Environment <fct> 1959 Emerald low control early, 1960 Emerald low control e~
#> $ Years       <int> 1959, 1960, 1961, 1962, 1959, 1960, 1961, 1962, 1959, 1960~
#> $ Sites       <fct> Emerald, Emerald, Emerald, Emerald, Yanco, Yanco, Yanco, Y~
#> $ Nitrogen    <fct> low, low, low, low, low, low, low, low, low, low, low, low~
#> $ CO2         <fct> control, control, control, control, control, control, cont~
#> $ Sowing      <fct> early, early, early, early, early, early, early, early, ea~

### calculate ecovalence for all genotypes
single.index.ecovalence <- ecovalence(data = Data,
                                      trait = 'Yield',
                                      genotype = 'Genotype',
                                      environment = 'Environment',
                                      unit.correct = FALSE,
                                      modify = FALSE)
### check the structure of result
dplyr::glimpse(single.index.ecovalence)
#> Rows: 5
#> Columns: 3
#> $ Genotype   <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.trait <dbl> 2878.070, 1913.365, 2911.395, 3038.426, 2024.919
#> $ ecovalence <dbl> 17802705, 27718900, 9365241, 12698454, 24133596

### calculate modified ecovalence for all genotypes
single.index.ecovalence.modified <- ecovalence(data = Data,
                                               trait = 'Yield',
                                               genotype = 'Genotype',
                                               environment = 'Environment',
                                               unit.correct = FALSE,
                                               modify = TRUE)
### check the structure of result
dplyr::glimpse(single.index.ecovalence.modified)
#> Rows: 5
#> Columns: 3
#> $ Genotype            <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.trait          <dbl> 2878.070, 1913.365, 2911.395, 3038.426, 2024.919
#> $ ecovalence.modified <dbl> 139083.63, 216553.91, 73165.94, 99206.67, 188543.72

### calculate all stability indices for all genotypes
summary.table <- table_stability(data = Data,
                                 trait = 'Yield',
                                 genotype = 'Genotype',
                                 environment = 'Environment',
                                 lambda = median(Data$Yield),
                                 normalize = FALSE,
                                 unit.correct = FALSE)
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : 
#> All of your genotypes didn't pass the Shapiro normality test!
#>  Safety_first Index may not be accurate.
#### warning message means your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(summary.table)
#> Rows: 5
#> Columns: 15
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395, 3038.4~
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
#> $ Safety.first.index                <dbl> 0.3523378, 0.6665326, 0.3242059, 0.2~
#> $ Coefficient.of.determination      <dbl> 0.9398731, 0.8270000, 0.9485154, 0.9~
#> $ Coefficient.of.regression         <dbl> 1.1596475, 0.8552736, 1.0316158, 1.1~
#> $ Deviation.mean.squares            <dbl> 108789.28, 193280.79, 73052.65, 8677~
#> $ Environmental.variance            <dbl> 1809327, 1117230, 1418923, 1630384, ~
#> $ Genotypic.stability               <dbl> 29248135, 24360429, 14583562, 214768~
#> $ Genotypic.superiority.measure     <dbl> 89307.69, 1004043.78, 70091.10, 3048~
#> $ Variance.of.rank                  <dbl> 1.770116, 2.281250, 1.561946, 1.7913~
#> $ Stability.variance                <dbl> 173448.30, 303582.09, 62720.42, 1064~
#> $ Adjusted.coefficient.of.variation <dbl> 50.31578, 47.87130, 44.31829, 46.565~
#> $ Ecovalence                        <dbl> 17802705, 27718900, 9365241, 1269845~
#> $ Ecovalence.modified               <dbl> 139083.63, 216553.91, 73165.94, 9920~

### calculate all stability indices for all genotypes
normalized.summary.table <- table_stability(data = Data,
                                            trait = 'Yield',
                                            genotype = 'Genotype',
                                            environment = 'Environment',
                                            lambda = median(Data$Yield),
                                            normalize = TRUE,
                                            unit.correct = FALSE)
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : 
#> All of your genotypes didn't pass the Shapiro normality test!
#>  Safety_first Index may not be accurate.
#### warning message means your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(normalized.summary.table)
#> Rows: 5
#> Columns: 15
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395, 3038.4~
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
#> $ Safety.first.index                <dbl> 0.85683453, 0.00000000, 0.93355270, ~
#> $ Coefficient.of.determination      <dbl> 0.07112157, 1.00000000, 0.00000000, ~
#> $ Coefficient.of.regression         <dbl> 0.0000000, 0.9787025, 0.4116811, 0.1~
#> $ Deviation.mean.squares            <dbl> 0.7027599, 0.0000000, 1.0000000, 0.8~
#> $ Environmental.variance            <dbl> 0.0000000, 0.9389617, 0.5296575, 0.2~
#> $ Genotypic.stability               <dbl> 0.0000000, 0.3333003, 1.0000000, 0.5~
#> $ Genotypic.superiority.measure     <dbl> 0.9395799, 0.0000000, 0.9593184, 1.0~
#> $ Variance.of.rank                  <dbl> 0.8095988, 0.3420919, 1.0000000, 0.7~
#> $ Stability.variance                <dbl> 0.5402844, 0.0000000, 1.0000000, 0.8~
#> $ Adjusted.coefficient.of.variation <dbl> 0.0000000, 0.4075840, 1.0000000, 0.6~
#> $ Ecovalence                        <dbl> 0.5402844, 0.0000000, 1.0000000, 0.8~
#> $ Ecovalence.modified               <dbl> 0.5402844, 0.0000000, 1.0000000, 0.8~

### compare the result from summary.table and normalized.summary.table


### calculate the stability indices only based only on CO2 and Nitrogen environments
summary.table2 <- table_stability(data = Data,
                                  trait = 'Yield',
                                  genotype = 'Genotype',
                                  environment = c('CO2','Nitrogen'),
                                  lambda = median(Data$Yield),
                                  normalize = FALSE,
                                  unit.correct = FALSE)
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : 
#> All of your genotypes didn't pass the Shapiro normality test!
#>  Safety_first Index may not be accurate.

#### check the structure of result
dplyr::glimpse(summary.table2)
#> Rows: 5
#> Columns: 15
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395, 3038.4~
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
#> $ Safety.first.index                <dbl> 0.3523378, 0.6665326, 0.3242059, 0.2~
#> $ Coefficient.of.determination      <dbl> 0.161086973, 0.138169855, 0.28644744~
#> $ Coefficient.of.regression         <dbl> 1.1791003, 0.8614393, 1.3780191, 1.3~
#> $ Deviation.mean.squares            <dbl> 1517867.6, 962862.6, 1012476.1, 1269~
#> $ Environmental.variance            <dbl> 1809327, 1117230, 1418923, 1630384, ~
#> $ Genotypic.stability               <dbl> 213741097, 130745446, 161091101, 189~
#> $ Genotypic.superiority.measure     <dbl> 3688981, 6251668, 3333615, 3180826, ~
#> $ Variance.of.rank                  <dbl> 2644.454, 1623.286, 2007.764, 2479.2~
#> $ Stability.variance                <dbl> 2025117, 1102709, 1229740, 1636649, ~
#> $ Adjusted.coefficient.of.variation <dbl> 50.31578, 47.87130, 44.31829, 46.565~
#> $ Ecovalence                        <dbl> 192140367, 121852817, 131532582, 162~
#> $ Ecovalence.modified               <dbl> 1501096.6, 951975.1, 1027598.3, 1269~

### compare the result from summary.table and summary.table2
### see how the choice of environments affect the data
```

<sup>Created on 2021-12-10 by the [reprex package](https://reprex.tidyverse.org) (v2.0.0)</sup>


```

## Enjoy
Hope you enjoy using this package when calculating the stability indices.
