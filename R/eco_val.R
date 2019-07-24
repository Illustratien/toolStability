# definied the global variable just for checking purpose when check the package
utils::globalVariables(c("XX", "mean.x","Genotype","d2","."),
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
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#' @references Wricke, G. "Uber eine Methode zur Erfassung der okologischen Streubreite in Feldverzuchen."
#'  \emph{Z. pflanzenzuchtg} 47 (1962): 92-96.
#'
#' @examples
#' data(Data)
#' eco_valance <- eco_val(Data,'Yield','Genotype','Environment')

eco_val <- function(data,trait,Genotype,Environment){
  # preprocessed the raw data
  Data <- data_prep(data,trait,Genotype,Environment)

  Data<- Data%>%
    dplyr::select(XX,mean.x,Genotype)%>%
    dplyr::group_by(Genotype)%>%
    dplyr::mutate(.,d2=(XX-mean.x)^2)%>%
    dplyr::summarise(envvar = sum(d2, na.rm=TRUE)/(length(d2)-1))
  return(Data)
}
