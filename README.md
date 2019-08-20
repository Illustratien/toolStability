# toolStability
This is a r package for calculating parametric, non-parametric, and probabilistic stability indices.

##package structure overview

This r package contain 12 different functions to calculate stability indices, including:

 1.  adjusted coefficient variaiton
 2.  coefficient of determination
 3.  coefficient of regression
 4.  deviation mean squares
 5.  ecovalence
 6.  environmental variance
 7.  genotypic stability
 8.  genotypic superiority measure
 9.  mean rank difference
 10. safty first index
 11. stability variance
 12. variance of rank


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
The default data is the subset of APSIM simulated wheat dataset, which includes 5 genotypes in 4 locations of 11 years 
(Casadebaig et al., 2016).

'Data' in this package is a dataframe with 220 observations on the following 3 variables.

Trait        Yield(kg.ha-1) 
Genotype     5 varieties
Environment  44 environments

## Enjoy
Hope you enjoy using this package when calculating the stability indices.
