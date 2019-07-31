#' @title Genotypic stability
#' 
#' @description
#' \code{genotypic_stability} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Genotypic stability is calculatd based on regression function.
#' Variety with low stability variance is considered as stable.
#' Under the linear model 
#' \deqn{Y =\mu + b_{i}e_{j} + g_{i} + d_{ij}}
#' where Y is the predicted phenotypic values, \eqn{g_{i}}, \eqn{e_{j}} and \eqn{\mu} denoting 
#' genotypic, environmental and overall popluation mean,respectively. 
#' 
#' The effect of GE-interaction may be expressed as:
#' \deqn{(ge)_{ij} = b_{i}e_{j} + d_{ij}}
#' where \eqn{b_{i}} is the coefficient of regression and \eqn{d_{ij}} a deviation.
#'
#' Genotypic stability(\link(\code(genotypic_stability))):
#' \deqn{ s^{2}_{di} = \frac{1}{E-2} \big[
#'  sum_{j}(X_{ij} -bar(X_{i.})- bar(X_{.j})+bar(X_{..})^{2} - (b_{i} - 1)^{2})
#'  (bar(X_{.j}-bar(X_{..})))^{2}} \big]
#' 
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} and  \eqn{bar(X)_{.j}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{bar(X)_{..}} denote the overall mean of X.
#'
#'
#' @param trait numeric vector of interested trait to be analysized.
#' @param genotype a character or factor vector labeling different genotypic varieties
#' @param environment a character or factor vector labeling different environments
#'
#' @return a data table with environmental variance indices
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{hanson1970}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' genotypic.stability <- genotypic_stability(Data$Yield,Data$Genotype,Data$Environment)
#'
genotypic_stability <- function(trait,genotype,environment){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  
  # combine vectors into data table
  Data <- data.table(X=trait,Genotype=genotype,Environment=environment)
  
  X..bar=mean(trait)               # overall mean of X
  res <- summarise(
    mutate(
      group_by(
        ungroup(
          mutate(
            group_by(Data,Environment),          # for each environment
            Xj.bar=mean(X))),                    # first calculate environmental mean
        Genotype),                               # for each genotype
      Xi.bar=mean(X),                            # then calculate genotypic mean
      Bi1=(X-Xi.bar-Xj.bar+X..bar)*(Xj.bar-X..bar),
      Bi2=(Xj.bar-X..bar)^2),
    Bi=1+(sum(Bi1,na.rm=TRUE)/sum(Bi2,na.rm=TRUE)),# end of mutate
    bmin=sum(Bi,na.rm=TRUE),
    genotypic.stability=sum(X-Xi.bar-bmin*Xj.bar+bmin*X..bar))# end of summarise
  return(res[,c('Genotype','genotypic.stability')])
}
