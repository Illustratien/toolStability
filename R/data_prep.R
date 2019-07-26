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
#' @importFrom magrittr %>%
#' @importFrom stats sd var
#' @importFrom data.table data.table
#' @return a dataframe with descriptive statics
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#'
#' @examples
#' data(Data)
#' result <- data_prep(Data,'Yield','Genotype','Environment')
#' str(result)#dataframe

data_prep <- function(data,trait,Genotype,Environment){
# use 'X' as abbreviation for simplyfying purpose
# in the latter equation for stability indices calculation
  names(data)[names(data) == trait] <- 'X'
  data <- data%>%data.table(key='Genotype,Environment,X')
  # Genotypic mean, sd, cv, log 10 of variance and mean
  GM <- data%>%
    dplyr::group_by(Genotype)%>%
    dplyr::summarise(
      Xi.bar=mean(data$X),
      Xi.sd=sd(data$X),
      Xi.cv=sd(data$X)/mean(data$X),
      Xi.logvar=log10(var(data$X)),
      Xi.logmean=log10(mean(data$X)))%>%
    data.table(key='Genotype')

  # Environmental mean, Xj
  EM<- data%>%
    dplyr::group_by(Environment)%>%
    dplyr::summarise(
      Xj.bar=mean(data$X),
      Xj.max=max(data$X))%>%
    data.table(key='Environment')

  # Combine genotypic and environmental data
  data <- merge(data, GM, by="Genotype")
  data <- merge(data, EM, by="Environment")

return(data)
}
