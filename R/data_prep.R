#' @title Data preprocessing
#'
#' @description
#' \code{data_prep} calculate genetic and environmental basic statstics required
#' for stability indices.
#'
#' @details
#' This function calculate mean, standard deviation (sd),correlation coefficient (cv),
#' log of variance (logvar), log of mean(logmean) for each genotype. \cr
#' Mean and maximum value were calculated for each environment.
#' @param data a dataframe containing at least three column of trait of interest,
#' genotype, and environment information.
#' @param trait name of the interested column in the dataframe to analysize.
#' @param Genotype genetic varieties
#' @param Environment environmental parameters
#' @export
#' @importFrom dplyr group_by summarise
#' @importFrom stats sd var
#' @importFrom data.table data.table
#' @return a dataframe with descriptive statics
#' @author Tien Cheng Wang
#'
#' @examples
#' data(Data)
#' result <- data_prep(Data,'Yield','Genotype','Environment')
#' str(result)#dataframe

data_prep <- function(data,trait,Genotype,Environment){
# use 'X' as abbreviation for simplyfying purpose
# in the latter equation for stability indices calculation
  names(data)[names(data) == trait] <- 'X'
  data <- data.table(data,key='Genotype,X,Environment')
  # Genotypic mean, sd, cv, log 10 of variance and mean
  GM <- data.table(
    summarise(
      group_by(data,Genotype),
      Xi.bar=mean(X),
      Xi.sd=sd(X),
      Xi.cv=sd(X)/mean(X),
      Xi.logvar=log10(var(X)),
      Xi.logmean=log10(mean(X))),
    key='Genotype')

  # Environmental mean, Xj
  EM <- data.table(
    summarise(
      group_by(data,Environment),
      Xj.bar=mean(X),Xj.max=max(X)),
    key='Environment')

  # Combine genotypic and environmental data
  data <- inner_join(data, GM, by="Genotype")
  data <- inner_join(data, EM, by="Environment")

return(data)
}
