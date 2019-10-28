
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
rm(list=ls())
library(toolStability)
### load data
data("Data")
### check the structure of sample dataset
### be sure that the trait is numeric!!!
dplyr::glimpse(Data)
#> Observations: 640
#> Variables: 8
#> $ Genotype    <fct> 1864, 1864, 1864, 1864, 1864, 1864, 1864, 1864, 18...
#> $ Yield       <dbl> 1278.6, 1746.0, 1753.9, 1851.8, 2176.6, 2783.3, 31...
#> $ Environment <fct> 1959 Emerald low control early, 1960 Emerald low c...
#> $ Years       <int> 1959, 1960, 1961, 1962, 1959, 1960, 1961, 1962, 19...
#> $ Sites       <fct> Emerald, Emerald, Emerald, Emerald, Yanco, Yanco, ...
#> $ Nitrogen    <fct> low, low, low, low, low, low, low, low, low, low, ...
#> $ CO2         <fct> control, control, control, control, control, contr...
#> $ Sowing      <fct> early, early, early, early, early, early, early, e...

### calculate ecovalence for all genotypes
single.index.ecovalence <- ecovalence(data = Data,
                               trait = 'Yield',
                               genotype = 'Genotype',
                               environment = 'Environment')
### check the structure of result
dplyr::glimpse(single.index.ecovalence)
#> Observations: 5
#> Variables: 2
#> $ Genotype   <fct> 1864, 2757, 2844, 3356, 4885
#> $ ecovalence <dbl> 139083.63, 216553.91, 73165.94, 99206.67, 188543.72

### calculate all stability indices for all genotypes
summary.table <- table_stability(data = Data,
                                 trait = 'Yield',
                                 genotype = 'Genotype',
                                 environment = 'Environment',
                                 lambda = median(Data$Yield),
                                 normalize = F )
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : All of your genotypes didn't pass the Shapiro normality test!
#>  Safety-first Index may not be accurate.
#### warning message is telling you that your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(summary.table)
#> Observations: 5
#> Variables: 14
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395...
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, ...
#> $ Safety.first.index                 <dbl> 0.3523378, 0.6665326, 0.3242...
#> $ Coefficient.of.determination      <dbl> 0.9398731, 0.8270000, 0.9485...
#> $ Coefficient.of.regression         <dbl> 1.1596475, 0.8552736, 1.0316...
#> $ Deviation.mean.squares            <dbl> 108789.28, 193280.79, 73052....
#> $ Environmental.variance            <dbl> 1809327, 1117230, 1418923, 1...
#> $ Genotypic.stability               <dbl> 29248135, 24360429, 14583562...
#> $ Genotypic.superiority.measure     <dbl> 89307.69, 1004043.78, 70091....
#> $ Variance.of.rank                  <dbl> 1.770116, 2.281250, 1.561946...
#> $ Stability.variance                <dbl> 173448.30, 303582.09, 62720....
#> $ Adjusted.coefficient.of.variation <dbl> 50.31578, 47.87130, 44.31829...
#> $ Ecovalence                        <dbl> 139083.63, 216553.91, 73165....

### calculate all stability indices for all genotypes
normalized.summary.table <- table_stability(data = Data,
                                 trait = 'Yield',
                                 genotype = 'Genotype',
                                 environment = 'Environment',
                                 lambda = median(Data$Yield),
                                 normalize = F )
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : All of your genotypes didn't pass the Shapiro normality test!
#>  Safety_first Index may not be accurate.
#### warning message is telling you that your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(normalized.summary.table)
#> Observations: 5
#> Variables: 14
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395...
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, ...
#> $ Safety.first.index                 <dbl> 0.3523378, 0.6665326, 0.3242...
#> $ Coefficient.of.determination      <dbl> 0.9398731, 0.8270000, 0.9485...
#> $ Coefficient.of.regression         <dbl> 1.1596475, 0.8552736, 1.0316...
#> $ Deviation.mean.squares            <dbl> 108789.28, 193280.79, 73052....
#> $ Environmental.variance            <dbl> 1809327, 1117230, 1418923, 1...
#> $ Genotypic.stability               <dbl> 29248135, 24360429, 14583562...
#> $ Genotypic.superiority.measure     <dbl> 89307.69, 1004043.78, 70091....
#> $ Variance.of.rank                  <dbl> 1.770116, 2.281250, 1.561946...
#> $ Stability.variance                <dbl> 173448.30, 303582.09, 62720....
#> $ Adjusted.coefficient.of.variation <dbl> 50.31578, 47.87130, 44.31829...
#> $ Ecovalence                        <dbl> 139083.63, 216553.91, 73165....

### compare the result from summary.table and normalized.summary.table


### calculate the stability indices only based only on CO2 and Nitrogen environments
summary.table2 <- table_stability(data = Data,
                                 trait = 'Yield',
                                 genotype = 'Genotype',
                                 environment = c('CO2','Nitrogen'),
                                 lambda = median(Data$Yield),
                                 normalize = F )
#> Warning in table_stability(data = Data, trait = "Yield", genotype = "Genotype", : All of your genotypes didn't pass the Shapiro normality test!
#>  Safety_first Index may not be accurate.

#### check the structure of result
dplyr::glimpse(summary.table2)
#> Observations: 5
#> Variables: 14
#> $ Genotype                          <fct> 1864, 2757, 2844, 3356, 4885
#> $ Mean.Yield                        <dbl> 2878.070, 1913.365, 2911.395...
#> $ Normality                         <lgl> FALSE, FALSE, FALSE, FALSE, ...
#> $ Safety.first.index                 <dbl> 0.3523378, 0.6665326, 0.3242...
#> $ Coefficient.of.determination      <dbl> 0.161086973, 0.138169855, 0....
#> $ Coefficient.of.regression         <dbl> 1.1791003, 0.8614393, 1.3780...
#> $ Deviation.mean.squares            <dbl> 1517867.6, 962862.6, 1012476...
#> $ Environmental.variance            <dbl> 1809327, 1117230, 1418923, 1...
#> $ Genotypic.stability               <dbl> 213741097, 130745446, 161091...
#> $ Genotypic.superiority.measure     <dbl> 3688981, 6251668, 3333615, 3...
#> $ Variance.of.rank                  <dbl> 2644.454, 1623.286, 2007.764...
#> $ Stability.variance                <dbl> 2025117, 1102709, 1229740, 1...
#> $ Adjusted.coefficient.of.variation <dbl> 50.31578, 47.87130, 44.31829...
#> $ Ecovalence                        <dbl> 1501096.6, 951975.1, 1027598...

### compare the result from summary.table and summary.table2
### see how the choice of environments affect the data
```

## Enjoy
Hope you enjoy using this package when calculating the stability indices.
