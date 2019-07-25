# definied the global variable just for checking purpose when check the package
utils::globalVariables(c("X", "Xi.bar","Genotype","d2","."),
                       package="toolStability")
#' @title Environmental variance
#'
#' @description
#' \code{env_var} calculate variance of a genotype across environments.
#'
#' @details
#' This function calculate environmental variance of genotypes with static concept. By detecting all
#' deviation from the genotypic mean. \cr The larger he environmental variance
#' one genotype is, the lower the stability.
#' \deqn{S^{2}_{xi} = \frac{\Simga_{j}(X_{ij} -bar(X)_{i.})^2}{E-1} }
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#' @param data a dataframe containing at least three column of trait of interest,
#'  genotype, and environment information.
#' @param trait the interested variable to be analysize.
#' @param Genotype a variable labeling different genotypic varieties
#' @param Environment a variable lableing different environmental parameters
#' @export
#' @importFrom magrittr %>%
#' @return a dataframe with environmental variance indices
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#' @references Roemer, J. "Sinde die ertagdreichen Sorten ertagissicherer."
#'  \emph{Mitt DLG} 32.1 (1917): 87-89.
#'
#' @examples
#' data(Data)
#' env_variance <- env_var(Data,'Yield','Genotype','Environment')

env_var <- function(data,trait,Genotype,Environment){
  # preprocessed the raw data
  Data <- data_prep(data,trait,Genotype,Environment)

  Data<- Data%>%
    dplyr::select(X,Xi.bar,Genotype)%>%
    dplyr::group_by(Genotype)%>%
    dplyr::mutate(.,d2=(X-Xi.bar)^2)%>%
    dplyr::summarise(env.var = sum(d2, na.rm=TRUE)/(length(d2)-1))
  return(Data)
}
