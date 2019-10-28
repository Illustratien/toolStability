# toolStability
This is a r package for calculating parametric, non-parametric, and probabilistic stability indices.

##package structure overview

This r package contain different functions to calculate stability indices, including:

 1.  adjusted coefficient variaiton
 2.  coefficient of determination
 3.  coefficient of regression
 4.  deviation mean squares
 5.  ecovalence
 6.  environmental variance
 7.  genotypic stability
 8.  genotypic superiority measure
 9. safty first index
 10. stability variance
 11. variance of rank


##function input tutorial

Most of the functions in the package work with the folling format: 
function(Data,'TraitName','Genotype','Environment')

In order to calculate stability index, you will need to prepare a dataframe containing trait, genotype,and environment:
  * trait:       colname of a column containing a numeric vector of interested trait to be analysized.
  * genotype:    colname of a column containing a character or factor vector labeling different genotypic varieties
  * environment: colname of a column containing a character or factor vector labeling different environments
  
In addition, for the safty first index, you need  
  * lambda:      threshold value of trait that define stability. 


We provide the additional function call table_stability, which provide the summary table containing all the stability indices
in the package for every genotypes, also including the mean yield and normality check results for the trait of eah genoytpes 
across all the enviornments.  

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

'Data' in this package is a dataframe with 220 observations on the following 3 variables.

Trait        Yield(kg.ha-1) 
Genotype     5 varieties
Environment  44 environments

## Enjoy
Hope you enjoy using this package when calculating the stability indices.
