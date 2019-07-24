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
#' @param trait the interested values to analysize.
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
# use 'XX' as abbreviation for simplyfying purpose
# in the latter equation for stability indices calculation
  names(data)[names(data) == trait] <- 'XX'
  data <- data%>%data.table(key='Genotype,Environment,XX')
  # Genotypic mean, sd, cv, log 10 of variance and mean
  GM <- data%>%
    dplyr::group_by(Genotype)%>%
    dplyr::summarise(
      mean=mean(data$XX),sd=sd(data$XX),cv=sd(data$XX)/mean(data$XX),
      logvar=log10(var(data$XX)),logmean=log10(mean(data$XX)))%>%
    data.table(key='Genotype')

  # Environmental mean, Xj
  EM<- data%>%
    dplyr::group_by(Environment)%>%
    dplyr::summarise(
      mean=mean(data$XX), max=max(data$XX))%>%
    data.table(key='Environment')

  # Rename the environmental statistics to differentiate from genotypic values
  names(EM)[names(EM) == "XX.mean"] <- 'XXj'
  names(EM)[names(EM) == "XX.max"] <- 'Xj.max'

  # Combine genotypic and environmental data
  data <- merge(data, EM, by="Environment")
  data <- merge(data, GM, by="Genotype")

return(data)
}
