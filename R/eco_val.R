# definied the global variable just for checking purpose when check the package
utils::globalVariables(c("X", "Xi.bar","Genotype","sqr","."),
                       package="toolStability")
#' @title Ecovalance
#'
#' @description
#' \code{eco_val} calculate genetic and environmental interaction.
#'
#' @details
#' This function calculate contribution of a genotype to the GE-interactions with dynamic concept, by
#' squared and summed across all environments for each genotype.
#'
#' \deqn{W_{i} = \Simga_{j}(X_{ij} -bar(X_{i.})_{i.})^2
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#'
#' @param data a dataframe containing at least three column of trait of interest,
#'  genotype, and environment information.
#' @param trait the interested variable to be analysize.
#' @param Genotype a variable labeling different genotypic varieties
#' @param Environment a variable lableing different environmental parameters
#' @export
#' @importFrom magrittr %>%
#' @return a dataframe with environmental variance indices
#' @importFrom Rdpack reprompt
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#' @references
#' \insertRef{wricke1962}{toolStability}
#'
#' @examples
#' data(Data)
#' eco_valance <- eco_val(Data,'Yield','Genotype','Environment')

eco_val <- function(data,trait,Genotype,Environment){
  # preprocessed the raw data
  Data <- data_prep(data,trait,Genotype,Environment)

  X..bar=mean(Data$X)
  Data<- Data%>%
    dplyr::select(X,Xi.bar,Xj.bar,Genotype)%>%
    dplyr::group_by(Genotype)%>%
    dplyr::mutate(.,sqr=(X-Xi.bar-Xj.bar+X..bar)^2)%>%
    dplyr::summarise(eco.val= mean(sqr, na.rm=TRUE))
  return(Data)
}
