utils::globalVariables(c('Bi','Bi1','Bi2','E','Environment','Genotype','Mean.Yield','Mj','X','Xi.bar','Xj.bar','Xj.max','corrected.X','corrected.rank','dev','deviation','mean.rank','s2d1','s2d2','s2di','s2xi','sqr','sqr1','wi'))
#' @title Environmental variance
#'
#' @description
#' \code{environmental_variance} is used to calculate variance of a genotype across environments.
#'
#' @keywords static stability
#'
#' @details
#' Environmental variance (Roemer 1917) is calculated by squared and suming up all deviation from genotypic mean for each genotype.
#' The larger the environmental variance of one genotype is, the lower the stability.
#'
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar(X)_{i.})^2}{E-1} }
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#'
#' @return a data table with environmental variance
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{roemer1917}{toolStability}\insertRef{shukla1972}{toolStability}
#'
#' @importFrom dplyr group_by summarise
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' env.var <- environmental_variance(Data$Yield,Data$Genotype)

environmental_variance <- function(trait,genotype){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}

    # combine vectors into data table
    Data <- data.table(X=trait,Genotype=genotype)

    #calculate environmental variance
    res <- summarise(
           mutate(group_by(Data,Genotype),#end of group_by
           deviation=(X-mean(X))^2/(length(X)-1)),
           environmental.variance = sum(deviation, na.rm=TRUE))#end of summarise

  return(res)
}
