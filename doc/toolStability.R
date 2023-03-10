## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  # Install from CRAN
#  install.packages('toolStability', dependencies=TRUE)
#  
#  # Install development version from Github
#  devtools::install_github("Illustratien/toolStability")
#  

## -----------------------------------------------------------------------------
library(toolStability)

## ----plot, fig.cap = 'Example of environmental effects on wheat yield.', echo = FALSE, fig.height=4, fig.width=8, message=FALSE----

ggplot2::ggplot(Data,ggplot2::aes(x=Sites,y=Yield,col=Genotype))+
  ggplot2::geom_boxplot()+
  ggplot2::facet_grid(Sowing~Nitrogen,labeller =ggplot2::label_both)+
  ggplot2::ylab(bquote('Wheat yield (kg' %.%'ha'^'-1'*')'))+
   ggplot2::theme_test()


## -----------------------------------------------------------------------------
rm(list=ls())
library(toolStability)
### load data
data("Data")
### check the structure of sample dataset
### be sure that the trait is numeric!!!



dplyr::glimpse(Data)

### calculate ecovalence for all genotypes
single.index.ecovalence <- ecovalence(data = Data,
                                      trait = 'Yield',
                                      genotype = 'Genotype',
                                      environment = 'Environment',
                                      unit.correct = FALSE,
                                      modify = FALSE)
### check the structure of result
dplyr::glimpse(single.index.ecovalence)

### calculate modified ecovalence for all genotypes
single.index.ecovalence.modified <- ecovalence(data = Data,
                                      trait = 'Yield',
                                      genotype = 'Genotype',
                                      environment = 'Environment',
                                      unit.correct = FALSE,
                                      modify = TRUE)
### check the structure of result
dplyr::glimpse(single.index.ecovalence.modified)

## -----------------------------------------------------------------------------
### calculate all stability indices for all genotypes
summary.table <- table_stability(data = Data,
                                 trait = 'Yield',
                                 genotype = 'Genotype',
                                 environment = 'Environment',
                                 lambda = median(Data$Yield),
                                 normalize = FALSE,
                                 unit.correct = FALSE)
#### warning message means your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(summary.table)

### calculate all stability indices for all genotypes
normalized.summary.table <- table_stability(data = Data,
                                            trait = 'Yield',
                                            genotype = 'Genotype',
                                            environment = 'Environment',
                                            lambda = median(Data$Yield),
                                            normalize = TRUE,
                                            unit.correct = FALSE)
#### warning message means your data structure is not distributed as normal distribution

#### check the structure of result
dplyr::glimpse(normalized.summary.table)

### compare the result from summary.table and normalized.summary.table


### calculate the stability indices only based only on CO2 and Nitrogen environments
summary.table2 <- table_stability(data = Data,
                                  trait = 'Yield',
                                  genotype = 'Genotype',
                                  environment = c('CO2','Nitrogen'),
                                  lambda = median(Data$Yield),
                                  normalize = FALSE,
                                  unit.correct = FALSE)

#### check the structure of result
dplyr::glimpse(summary.table2)

### compare the result from summary.table and summary.table2
### see how the choice of environments affect the data


