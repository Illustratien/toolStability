#' @title Environmental variance
#'
#' @description
#' \code{env_var} is used to calculate variance of a genotype across environments.
#' @keywords static stability
#'
#' @details
#' Environmental variance (Roemer 1917) is calculated based on suming up all deviation from genotypic mean for each genotype.
#' The larger the environmental variance of one genotype is, the lower the stability.
#'
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar(X)_{i.})^2}{E-1} }
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#' @param trait numeric vector of interested trait to be analysized.
#' @param genotype a character or factor vector labeling different genotypic varieties
#' @param ... data table with precalculated statistic procuded by \code{\link{stability_indices_table}}of to be passed
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by mutate summarise
#' @importFrom Rdpack reprompt
#' @return a data table with environmental variance indices
#' @author Tien Cheng Wang
#' @references
#' \insertRef{roemer1917}{toolStability}
#' @examples
#' data(Data)
#' env.var <- environmental_variance(Data$'Yield',Data$Genotype)

environmental_variance <- function(trait,genotype){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
    # combine vectors into data table
    Data <- data.table(X=trait,Genotype=genotype)
    #calculate environmental variance
    res <- summarise(group_by(Data,Genotype),#end of group_by
      env.var = sum(var(X), na.rm=TRUE)/(length(unique(Genotype)-1)))#end of summarise
  return(res)
}
